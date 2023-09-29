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
  simulation <- read.csv(paste0("./sim_1.5_1.0_0.01_0.001/simulation_",i,".csv")) %>% tibble::as_tibble()
  print(paste0("simulation_",i))
  samples <- simulation[seq(3,21, by = 9),]
  data_list <- list(
    n_times = nrow(samples),
    z0 = c(1000,100,0,0,0),
    t0 = simulation$time[1],
    zminus = samples$z_minus,
    zplus = samples$z_plus,
    t = samples$time
  )
  fit <- rstan::sampling(model, data_list, chains=4, warmup=4000, iter=8000, cores=4)
  saveRDS(fit,paste0("./fit_1.5_1.0_0.01_0.001_3_500_0.02/fit_",i,".rds"))

}
simulation <- read.csv(paste0("./sim_1.5_1.0_0.01_0.001/simulation_",3,".csv")) %>% tibble::as_tibble()
samples <- simulation[seq(3,21, by = 9),]
fit <- readRDS("./fit_1.5_1.0_0.01_0.001_3_500_0.01/fit_3.rds")
print(fit, digits_summary = 5)
# stan_fit(1)
# 
# fit1 = readRDS("./fit_3/fit_1.rds")
# print(fit1, pars = c("lambda_minus", "lambda_plus", "omega_minus", "omega_plus"), digits_summary = 3)
prova <- lapply(0:2, stan_fit)

easypar::run( FUN = stan_fit,
              PARAMS = lapply(0:10, list),
              parallel = TRUE,
              filter_errors = FALSE
)
