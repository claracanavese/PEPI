library(rstan)
library(magrittr)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(StanHeaders)

model <- rstan::stan_model("./PEPI/regressionODE.stan")

stan_fit = function(i){
  
  model <- rstan::stan_model("./PEPI/regressionODE.stan")
  simulation <- readRDS(paste0("./R/simulations/simulation_",i,".rds")) %>% tibble::as_tibble()
  samples <- simulation[seq(12,82, by = 7),]
  data_list <- list(
    n_times = nrow(samples),
    z0 = c(1000,100,0,0,0),
    t0 = simulation$time[1],
    zminus = as.integer(samples$zm),
    zplus = as.integer(samples$zp),
    t = samples$time
  )
  fit <- rstan::sampling(model, data_list, chains=4, warmup=6000, iter=12000, cores=4)
  saveRDS(fit,paste0("./R/fit/fit_",i,".rds"))

}

stan_fit(1)

fit1 = readRDS("./R/fit/fit_1.rds")
print(fit1, pars = c("lambda_minus", "lambda_plus", "omega_minus", "omega_plus"), digits_summary = 3)

easypar::run( FUN = stan_fit,
              PARAMS = lapply(1:3, list),
              parallel = TRUE,
              filter_errors = FALSE
)
