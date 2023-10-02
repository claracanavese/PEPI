library(rstan)
library(magrittr)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(StanHeaders)
library(easypar)

# model <- rstan::stan_model("./PEPI/regressionODE.stan")
# simulation <- readRDS(paste0("./PEPI/simulations/simulation_",1,".rds")) %>% tibble::as_tibble()

stan_fit = function(i){
  
  model <- rstan::stan_model("./regressionODE.stan")
  simulation1 <- read.csv(paste0("./simulations/sim_1.5_1.0_0.01_0.001/simulation_",i,".csv")) %>% tibble::as_tibble()
  simulation2 <- read.csv(paste0("./simulations/sim_1.2_0.01_0.001/simulation_",1,".csv")) %>% tibble::as_tibble()
  simulation3 <- read.csv(paste0("./simulations/sim_1.0_1.5_0.01_0.001/simulation_",2,".csv")) %>% tibble::as_tibble()
  print(paste0("simulation_",i))
  samples1 <- simulation1[c(2,4,5,7,8,10,11,13,15,16,18,19,21),]
  samples2 <- simulation2[c(2,4,5,7,8,10,11,13,15,16,18,19,21),]
  samples3 <- simulation3[seq(3,51, by = 4),]
  data_list1 <- list(
    n_times = nrow(samples1),
    z0 = c(1000,100,0,0,0),
    t0 = simulation1$time[1],
    zminus = samples1$z_minus,
    zplus = samples1$z_plus,
    t = samples1$time
  )
  fit1 <- rstan::sampling(model, data_list1, chains=4, warmup=4000, iter=8000, cores=4)
  data_list2 <- list(
    n_times = nrow(samples2),
    z0 = c(1000,100,0,0,0),
    t0 = simulation2$time[1],
    zminus = samples2$z_minus,
    zplus = samples2$z_plus,
    t = samples2$time
  )
  fit2 <- rstan::sampling(model, data_list2, chains=4, warmup=4000, iter=8000, cores=4)
  data_list3 <- list(
    n_times = nrow(samples3),
    z0 = c(1000,100,0,0,0),
    t0 = simulation3$time[1],
    zminus = samples3$z_minus,
    zplus = samples3$z_plus,
    t = samples3$time
  )
  fit3 <- rstan::sampling(model, data_list3, chains=4, warmup=4000, iter=8000, cores=4)
  saveRDS(fit1,paste0("/home/ccanavese/dati_ccanavese/GitHub/fit_13/fit_1.5_1.0_0.01_0.001/fit_",i,".rds"))
  saveRDS(fit2,paste0("/home/ccanavese/dati_ccanavese/GitHub/fit_13/fit_1.2_0.01_0.001/fit_",i,".rds"))
  saveRDS(fit3,paste0("/home/ccanavese/dati_ccanavese/GitHub/fit_13/fit_1.0_1.5_0.01_0.001/fit_",i,".rds"))
}

# stan_fit(1)
# 
# fit = readRDS("/home/ccanavese/dati_ccanavese/GitHub/fit_8/fit_1.0_1.5_0.01_0.001/fit_2.rds")
# print(fit, digits_summary = 3)

easypar::run( FUN = stan_fit,
              PARAMS = lapply(0:100, list),
              parallel = TRUE,
              filter_errors = FALSE
)
