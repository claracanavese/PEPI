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
  
  model <- rstan::stan_model("/Users/claracanavese/Documents/PEPI/PEPI/regressionODE.stan")
  simulation1 <- read.csv(paste0("./sim_1.5_1.0_0.001_0.01/simulation_",i,".csv")) %>% tibble::as_tibble()
  simulation2 <- read.csv(paste0("./sim_1.2_0.001_0.01/simulation_",i,".csv")) %>% tibble::as_tibble()
  simulation3 <- read.csv(paste0("./sim_1.0_1.5_0.001_0.01/simulation_",i,".csv")) %>% tibble::as_tibble()
  print(paste0("simulation_",i))
  samples1 <- simulation1[c(2,5,8,11,14,17,19,21),]
  samples2 <- simulation2[c(2,5,8,11,14,17,19,21),]
  samples3 <- simulation3[c(2,5,8,11,14,17,19,21),]
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
  saveRDS(fit1,paste0("./fit_8/fit_1.5_1.0_0.001_0.01/fit_",i,".rds"))
  saveRDS(fit2,paste0("./fit_8/fit_1.2_0.001_0.01/fit_",i,".rds"))
  saveRDS(fit3,paste0("./fit_8/fit_1.0_1.5_0.001_0.01/fit_",i,".rds"))

}

simulation4 <- read.csv(paste0("./sim_1.5_1.0_0.01_0.001/simulation_",4,".csv")) %>% tibble::as_tibble()
simulation5 <- read.csv(paste0("./sim_1.5_1.0_0.005/simulation_",5,".csv")) %>% tibble::as_tibble()


samples4 <- simulation4[c(1,12,21),]
samples5 <- simulation5[c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,51),]

fit4 <- readRDS("./fit_3/fit_1.5_1.0_0.01_0.001/fit_4.rds")
fit5 <- readRDS("./fit_3/fit_1.5_1.0_0.01_0.001/fit_5.rds")
# fit33 <- readRDS("./fit_1.0_1.5_0.01_0.001/fit_33.rds")
print(fit4, digits_summary = 5)
# stan_fit(1)
# 
# fit1 = readRDS("./fit_3/fit_1.rds")
# print(fit1, pars = c("lambda_minus", "lambda_plus", "omega_minus", "omega_plus"), digits_summary = 3)
prova <- lapply(0:2, stan_fit)

easypar::run( FUN = stan_fit,
              PARAMS = lapply(0:100, list),
              parallel = TRUE,
              filter_errors = FALSE
)
