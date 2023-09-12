library(rstan)
library(magrittr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(patchwork)
library(bayesplot)

simulation_py <- read.csv("./sim_py_1/sim_1.csv") %>% tibble::as_tibble()
simulation_R <- readRDS("./PEPI/simulations/simulation_1.rds") %>% tibble::as_tibble()
# colnames(simulation_py) <- c('time','z_minus','z_plus','var_minus','var_plus','cov')
samples <- simulation_py[seq(12,82, by = 7),]

simulation_py <- simulation_py[,-1]
t_samples = seq(1.00, 8.00, by = 0.70) %>% round(., 3)
samples = simulation_py %>% filter(simulation_py$time %in% t_samples)

prior_lambda = ggplot() + stat_function(fun=dgamma, args = list(shape = 2., rate = 1.)) +
  xlim(0,5) + ggtitle("Prior") + xlab("lambda") + theme(plot.title = element_text(hjust = 0.5)) + ylab("density")
# prior_omega = ggplot() + stat_function(fun=dcauchy, args = list(location = 0.01, scale = 0.01)) +
#   ggtitle("Prior") + xlab("omega") + theme(plot.title = element_text(hjust = 0.5)) + ylab("density") + xlim(0.,0.03)
prior_omega = 
  ggplot() + stat_function(fun = dgamma, args = list(shape = 1.5, rate = 200)) +
  ggtitle("Prior") + xlab("omega") + theme(plot.title = element_text(hjust = 0.5)) + ylab("density") + xlim(0.,0.03)
  # geom_vline(xintercept = 0.01, color = "red") + geom_vline(xintercept = 0.001, color = "green")
# ggplot() +
#   stat_function(fun=dgamma, args = list(shape = 1.5, rate = 280), color = "red") + xlim(0,0.03)

ggplot() + stat_function(fun = dgamma, args = list(shape = 1.5, rate = 280), color = "red") +
stat_function(fun = dgamma, args = list(shape = 2, rate = 280),color = "green") +
stat_function(fun = dgamma, args = list(shape = 1.5, rate = 200)) + xlim(0,0.05)

# NEW MODEL
data_list <- list(
  n_times = nrow(samples),
  z0 = c(1000,100,0,0,0),
  t0 = simulation_py$time[1],
  zminus = as.integer(samples$z_minus),
  zplus = as.integer(samples$z_plus),
  t = samples$time
)
model <- rstan::stan_model("./PEPI/regressionODE.stan")
fit <- rstan::sampling(model, data_list, chains=4, warmup=6000, iter=12000, cores=4)

print(fit, pars = c("lambda_minus", "lambda_plus", "omega_minus", "omega_plus"), digits_summary = 5)
print(fit, pars = c("lambda_minus"), digits_summary = 5)
print(fit) 
# saveRDS(fit,"./fit_11/fit_359.rds")

# simulation_py <- read.csv("./Gillespy2/8t/1.2_1.5_0.001_0.01_8t_81p/simulations/switching_results_0.csv") %>% tibble::as_tibble()
# simulation_py <- simulation_py[,-1]
# t_samples = seq(1.0, 8.0, by = 0.7) %>% round(., 3)
# samples = simulation_py %>% filter(simulation_py$time %in% t_samples)

# fit = readRDS("./fit_10/fit_1.rds")
# print(fit, pars = c("lambda_minus", "lambda_plus", "omega_minus", "omega_plus"), digits_summary = 3)

bayesplot::mcmc_trace(fit, pars = c("lambda_minus", "lambda_plus", "omega_minus", "omega_plus"))
ggsave("./GitHub/switching_process/Gillespy2/1.5_1.2_0.015_0.005_5t_51p/gamma_1.5_280/traceplot_0.png", width = 14, height = 12, dpi = 600)

# options(scipen = 1)
# color_scheme_set("pink")
minuspred = bayesplot::ppc_intervals(
  y = samples$z_minus,
  yrep = rstan::extract(fit, pars = c("pred_minus"))$pred_minus %>% as.matrix(),
  x = samples$time,
  prob = 0.5
) + xlab("t") + ylab("z-") + scale_x_continuous(breaks = pretty) + scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
#ggsave("./GitHub/switching_process/Gillespy2/1.5_1.0_005_001/beta_2_80/zminus_pred.png", width = 10, height = 7, dpi = 600)
minuspred

# errore % su y_min
y_min = samples$zm
yrep_min = rstan::extract(fit, pars = c("pred_minus"))$pred_minus %>% as.data.frame()
err_min = 0
for (i in 1:nrow(samples)) {
  err_min = err_min + abs((y_min[i] - median(yrep_min[,i])))/y_min[i]
}
err_min = err_min/length(nrow(samples))

pluspred = bayesplot::ppc_intervals(
  y = samples$z_plus,
  yrep = rstan::extract(fit, pars = c("pred_plus"))$pred_plus %>% as.matrix(),
  x = samples$time,
  prob = 0.5
) + xlab("t") + ylab("z+") + scale_x_continuous(breaks = pretty) + scale_y_continuous(labels = function(x) format(x, scientific = TRUE))     
#ggsave("./GitHub/switching_process/Gillespy2/1.5_1.0_005_001/beta_2_80/zplus_pred.png", width = 10, height = 7, dpi = 600)
pluspred

# errore % su y_plus
y_plus = samples$z_plus
yrep_plus = rstan::extract(fit, pars = c("pred_plus"))$pred_plus %>% as.data.frame()
err_plus = 0
for (i in 1:nrow(samples)) {
  err_plus = err_plus + abs((y_plus[i] - median(yrep_plus[,i])))/y_plus[i]
}
err_plus = err_plus/length(nrow(samples))

posterior = as.data.frame(fit)
posterior_lambda_min = posterior %>% ggplot() + geom_density(aes(x = lambda_minus, y = after_stat(density))) + ggtitle("Posterior") + xlim(0,5) + xlab("lambda_minus") + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = 1.5, color = "forestgreen")
posterior_lambda_plus = posterior %>% ggplot() + geom_density(aes(x = lambda_plus, y = after_stat(density))) + ggtitle("Posterior") + xlim(0,5) + xlab("lambda_plus") + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = 1.0, color = "forestgreen")
posterior_omega_min = posterior %>% ggplot() + geom_density(aes(x = omega_minus, y = after_stat(density))) + ggtitle("Posterior") + xlim(0,0.03) + xlab("omega_minus") + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = 0.01, color = "forestgreen")
posterior_omega_plus = posterior %>% ggplot() + geom_density(aes(x = omega_plus, y = after_stat(density))) + ggtitle("Posterior") + xlim(0,0.03) + xlab("omega_plus") + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = 0.001, color = "forestgreen")

posterior_lambda_min / prior_lambda
#ggsave("./GitHub/switching_process/Gillespy2/1.5_1.0_005_001/beta_2_80/lambda_minus_posterior.png", width = 12, height = 7, dpi = 600)
posterior_lambda_plus / prior_lambda
#ggsave("./GitHub/switching_process/Gillespy2/1.5_1.0_005_001/beta_2_80/lambda_plus_posterior.png", width = 12, height = 7, dpi = 600)
posterior_omega_min / prior_omega
#ggsave("./GitHub/switching_process/Gillespy2/1.5_1.0_005_001/beta_2_80/omega_minus_posterior.png", width = 12, height = 7, dpi = 600)
posterior_omega_plus / prior_omega
#ggsave("./GitHub/switching_process/Gillespy2/1.5_1.0_005_001/beta_2_80/omega_plus_posterior.png", width = 12, height = 7, dpi = 600)

(minuspred + pluspred) / (posterior_lambda_min + posterior_lambda_plus) / (prior_lambda + prior_lambda) / (posterior_omega_min + posterior_omega_plus) / (prior_omega + prior_omega)
ggsave("./GitHub/switching_process/Gillespy2/5t/1.2_1.5_0.01_0.001_5t_51p/gamma_1.5_280/panel_avg.png", width = 12, height = 14, dpi = 600)

# omega posteriors on same plot
fit1 = readRDS("./Gillespy2/8t/1.5_1.2_0.015_0.005_8t_81p/gamma_1.5_280/fit_0.rds")
print(fit7, pars = c("lambda_minus", "lambda_plus", "omega_minus", "omega_plus"), digits_summary = 3)
fit2 = readRDS("./Gillespy2/8t/1.5_1.2_0.015_0.005_8t_81p/gamma_1.5_280/fit_1.rds")
fit3 = readRDS("./Gillespy2/8t/1.5_1.2_0.015_0.005_8t_81p/gamma_1.5_280/fit_2.rds")
fit4 = readRDS("./Gillespy2/8t/1.5_1.2_0.015_0.005_8t_81p/gamma_1.5_280/fit_3.rds")
fit5 = readRDS("./Gillespy2/8t/1.5_1.2_0.015_0.005_8t_81p/gamma_1.5_280/fit_4.rds")
fit6 = readRDS("./Gillespy2/8t/1.5_1.2_0.015_0.005_8t_81p/gamma_1.5_280/fit_avg.rds")
fit7 = readRDS("./Gillespy2/8t/1.5_1.2_0.015_0.005_8t_81p/gamma_1.5_280/fit_ode.rds")
posterior1 = as.data.frame(fit1); posterior2 = as.data.frame(fit2); posterior3 = as.data.frame(fit3); posterior4 = as.data.frame(fit4); posterior5 = as.data.frame(fit5); posterior6 = as.data.frame(fit6); posterior7 = as.data.frame(fit7)
posterior_omega_min = ggplot() + 
  geom_density(data = posterior1, aes(x = omega_minus, y = after_stat(density), colour = "0")) +
  geom_density(data = posterior2, aes(x = omega_minus, y = after_stat(density), colour = "1")) +
  geom_density(data = posterior3, aes(x = omega_minus, y = after_stat(density), colour = "2")) +
  geom_density(data = posterior4, aes(x = omega_minus, y = after_stat(density), colour = "3")) +
  geom_density(data = posterior5, aes(x = omega_minus, y = after_stat(density), colour = "4")) +
  geom_density(data = posterior6, aes(x = omega_minus, y = after_stat(density), colour = "avg")) +
  geom_density(data = posterior7, aes(x = omega_minus, y = after_stat(density), colour = "ode")) +
  ggtitle("Posterior") + xlim(0,0.03) + xlab("omega_minus") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_discrete(name = "") +
  geom_vline(xintercept = 0.005, color = "black")
posterior_omega_plus = ggplot() + 
  geom_density(data = posterior1, aes(x = omega_plus, y = after_stat(density), colour = "0")) +
  geom_density(data = posterior2, aes(x = omega_plus, y = after_stat(density), colour = "1")) +
  geom_density(data = posterior3, aes(x = omega_plus, y = after_stat(density), colour = "2")) +
  geom_density(data = posterior4, aes(x = omega_plus, y = after_stat(density), colour = "3")) +
  geom_density(data = posterior5, aes(x = omega_plus, y = after_stat(density), colour = "4")) +
  geom_density(data = posterior6, aes(x = omega_plus, y = after_stat(density), colour = "avg")) +
  geom_density(data = posterior7, aes(x = omega_plus, y = after_stat(density), colour = "ode")) +
  ggtitle("Posterior") + xlim(0,0.03) + xlab("omega_plus") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_discrete(name = "") +
  geom_vline(xintercept = 0.015, color = "black")

(posterior_omega_min + posterior_omega_plus) / (prior_omega + prior_omega)
ggsave("./Gillespy2/8t/1.5_1.2_0.015_0.005_8t_81p/omega_posterior_gamma_1.5_280.png", width = 14, height = 10, dpi = 600)


# OLD MODEL
data_list <- list(
  n_times = nrow(samples),
  t = samples$time,
  zminus = as.integer(samples$z_minus),
  zplus = as.integer(samples$z_plus),
  z0 = c(1000,100)
)

model <- rstan::stan_model("./GitHub/switching_process/regression.stan")
# fit <- rstan::sampling(model, data_list, init = list(list(lambda_minus = 15, lambda_plus=10, omega_plus = 0.1, omega_minus = 0.01),list(lambda_minus = 15, lambda_plus=10, omega_plus = 0.1, omega_minus = 0.01),list(lambda_minus = 15, lambda_plus=10, omega_plus = 0.1, omega_minus = 0.01),list(lambda_minus = 15, lambda_plus=10, omega_plus = 0.1, omega_minus = 0.01)), chains=4, warmup=3000, iter = 6000)
fit <- rstan::sampling(model, data_list, chains=4, warmup=5000, iter=10000, cores=4)

bayesplot::mcmc_trace(fit, pars = c("lambda_minus","lambda_plus","omega_minus","omega_plus"))
print(fit, pars = c("lambda_minus","lambda_plus","omega_minus","omega_plus"), digits_summary = 3)
print(fit, digits_summary = 1e-8)


rstan::extract(fit, pars=c("omega_plus"))
bayesplot::mcmc_areas(fit, pars = c("lambda_minus"))
bayesplot::mcmc_areas(fit, pars = c("lambda_plus"))
bayesplot::mcmc_areas(fit, pars = c("omega_minus"))
bayesplot::mcmc_areas(fit, pars = c("omega_plus"))

prior_lambda = ggplot() +
  stat_function(fun=dgamma, args = list(shape = 2., rate = 1.)) +
  xlim(0,5) + ggtitle("Prior")
prior_omega = ggplot() +
  stat_function(fun=dgamma, args = list(shape = 2., rate = 1/0.005)) +
  xlim(0.,0.1) + ggtitle("Prior")

ggplot() +
  stat_function(fun=dgamma, args = list(shape = 2., rate = 100.)) +
  xlim(0.,0.2) + ggtitle("Prior")

bayesplot::mcmc_trace(fit, pars = c("lambda_minus"))
bayesplot::mcmc_trace(fit, pars = c("lambda_plus"))
bayesplot::mcmc_trace(fit, pars = c("omega_plus"))
bayesplot::mcmc_trace(fit, pars = c("omega_minus"))

bayesplot::ppc_intervals(
  y = samples$z_minus,
  yrep = rstan::extract(fit, pars = c("pred_minus"))$pred_minus %>% as.matrix(),
  x = samples$time,
  prob = 0.5
) +
  bayesplot::ppc_intervals(
  y = samples$z_plus,
  yrep = rstan::extract(fit, pars = c("pred_plus"))$pred_plus %>% as.matrix(),
  x = samples$time,
  prob = 0.5
)

posterior = as.data.frame(fit1)

# plot samples from PRIOR
y_prior = posterior %>% dplyr::select(starts_with("y_prior"))
y_prior <- reshape2::melt(y_prior)
ggplot(y_prior) + geom_density(aes(x=value,y=after_stat(density))) + xlim(-0.2,0.2)#+ stat_function(fun=dgamma, args = list(shape = 8.5, rate = 1./1.8))

# POSTERIOR vs PRIOR
posterior_lambda_min = posterior %>% ggplot() + geom_density(aes(x = lambda_minus, y = after_stat(density))) + ggtitle("Posterior") + xlim(0,5)
posterior_lambda_plus = posterior %>% ggplot() + geom_density(aes(x = lambda_plus, y = after_stat(density))) + ggtitle("Posterior") + xlim(0,5)
posterior_omega_min = posterior %>% ggplot() + geom_density(aes(x = omega_minus, y = after_stat(density))) + ggtitle("Posterior") + xlim(0,0.3)
posterior_omega_plus = posterior %>% ggplot() + geom_density(aes(x = omega_plus, y = after_stat(density))) + ggtitle("Posterior") + xlim(0,0.3)

posterior_lambda_min / prior_lambda
posterior_lambda_plus / prior_lambda
posterior_omega_min / prior_omega
posterior_omega_plus / prior_omega

ggplot() +
  geom_density(data = posterior, aes(x = lambda_minus, y = after_stat(density))) +
  stat_function(fun=dgamma, args = list(shape = 8.5, rate = 1.8)) 

posterior <- posterior[,1:4]
mcmc_intervals(posterior,)

x = posterior %>% dplyr::select(starts_with("pred_minus"))
x <- x[nrow(x),]
x <- reshape2::melt(x)
# x[,-1]
min_pred_df <- data.frame(time = t_samples, z_min_pred = x[,-1])
ggplot(min_pred_df, aes(x = time, y = z_min_pred)) + geom_point()

y = posterior %>% dplyr::select(starts_with("pred_plus"))
y <- y[nrow(y),]
y <- reshape2::melt(y)
plus_pred_df <- data.frame(time = t_samples, z_plus_pred = y[,-1])
ggplot(plus_pred_df, aes(x = time, y = z_plus_pred)) + geom_point()

ggplot() +
  geom_line(data = simulation_py, aes(x = time, y = z_minus),  color = "forestgreen") +
  geom_point(data = min_pred_df, aes(x = time, y = z_min_pred)) +
  xlim(0.5,1)

ggplot() +
  geom_line(data = simulation_py, aes(x = time, y = z_plus),  color = "forestgreen") +
  geom_point(data = plus_pred_df, aes(x = time, y = z_plus_pred)) +
  xlim(0.5,1)
