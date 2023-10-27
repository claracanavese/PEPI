library(rstan)
library(magrittr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(patchwork)
library(bayesplot)

simulation_py <- read.csv("./simulation_1.2_0.01_0.001_101p.csv") %>% tibble::as_tibble()
# colnames(simulation_py) <- c('time','z_minus','z_plus','var_minus','var_plus','cov')
samples <- simulation_py[c(2,3,4,5,6,7,8,10,11,12,14,15,16,17,18,19,20,21),]
samples <- simulation_py[c(2,5,8,11,14,16,19,21),]
samples <- simulation_py[2:101,]
# t_samples = seq(1.00, 8.00, by = 0.70) %>% round(., 3)
# samples = simulation_py %>% filter(simulation_py$time %in% t_samples)

prior_lambda = ggplot() + 
  #stat_function(fun=dgamma, args = list(shape = 2., rate = 1.), linewidth = 1) +
  stat_function(fun=dgamma, args = list(shape = 1200, rate = 1000), linewidth = 1) +
  xlim(0,3) + 
  ggtitle(expression("Gamma"~"("*alpha*" = 2, "*beta*" = 1.5)")) + 
  theme(plot.title = element_text(size = 14)) +
  xlab(expression(lambda)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab(expression(p(lambda))) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) 
  # geom_vline(xintercept = 1.0, color = "#379237", linewidth = 0.8) +
  # geom_vline(xintercept = 1.2, color = "#54B435", linewidth = 0.8) +
  # geom_vline(xintercept = 1.5, color = "#82CD47", linewidth = 0.8)

gamma_wide = data.frame( "gamma" = rgamma(10^6,shape=3,rate=500))
gamma_narrow = data.frame("gamma" = rgamma(10^6,shape=1,rate=1000))

prior_omega_p = ggplot() + 
  geom_density(data = gamma_narrow, aes(x = gamma, y = after_stat(ndensity))) 
  stat_function(fun = dgamma, args = list(shape = 1, rate = 1000), linewidth = 1) +
  ggtitle(expression("Gamma"~"("*alpha*" = 1, "*beta*" = 1000)")) + 
  theme(plot.title = element_text(size = 14)) +
  xlab(expression(hat(omega))) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab(expression(p(hat(omega)))) + 
  scale_x_continuous(limits = c(0,0.02), breaks = c(0,0.005, 0.01,0.015,0.02)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) 
  # geom_vline(xintercept = .001, color = "#7b2cbf", linewidth = 0.8)

prior_omega = ggplot() + 
  stat_function(fun = dgamma, args = list(shape = 42, rate = 5000), linewidth = 1) +
  # stat_function(fun = dgamma, args = list(shape = 3, rate = 400), color = "darkred", linewidth = 1) +
  ggtitle(expression("Gamma"~"("*alpha*" = 3, "*beta*" = 500)")) + 
  theme(plot.title = element_text(size = 16)) +
  xlab(expression(hat(omega))) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab(expression(p(hat(omega)))) + 
  scale_x_continuous(limits = c(0,0.02), breaks = c(0,0.005, 0.01,0.015,0.02)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) 
  # geom_vline(xintercept = .005, color = "#9d4edd", linewidth = 0.8) +
  # geom_vline(xintercept = .01, color = "#c77dff", linewidth = 0.8)

prior_lambda + prior_omega + prior_omega_p
ggsave("./priors.png",width = 15, height = 4, dpi = 600)

# NEW MODEL

data_list <- list(
  n_times = nrow(samples),
  z0 = c(1000,100,0,0,0),
  t0 = simulation_py$time[1],
  zminus = samples$z_minus,
  zplus = samples$z_plus,
  t = samples$time
)

# data_list <- list(
#   n_times = nrow(input)-1,
#   z0 = c(1000,100,0,0,0),
#   t0 = input$t[1],
#   zminus = input$zmin[2:nrow(input)],
#   zplus = input$zplus[2:nrow(input)],
#   t = input$t[2:nrow(input)]
# )

#model <- rstan::stan_model("./PEPI/old/regression.stan")
model <- rstan::stan_model("./regressionODE.stan")
fit <- rstan::sampling(model, data_list, chains=4, warmup=4000, iter=8000, cores=4)
# fit1 <- readRDS("./fit_new_ode/ssa - 10/1.2_0.01_0.001_sim4.rds")

fit_g = readRDS("./fit_8/fit_1.5_1.0_0.001_0.01/fit_50.rds")
fit3 = readRDS("./fit_3/fit_1.2_0.01_0.001/fit_50.rds")
fit18 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_50.rds")
fit100 = readRDS("./fit_100.rds")
fit100_ric = readRDS("./fit_100_RIC.rds")
fit100_new = readRDS("./fit_100_new.rds")
fit100_new_ric = readRDS("./fit_100_new_RIC.rds")
# fit27 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_27.rds")
print(fit100_new, pars = c("lambda_minus", "lambda_plus", "rate_minus", "rate_plus", "theta[3]", "theta[4]"), digits_summary = 5)
print(fit18, digits_summary = 5)
saveRDS(fit,"./fit_100_new.rds")

bayesplot::mcmc_trace(fit, pars = c("lambda_minus", "lambda_plus", "rate_minus", "rate_plus"))
ggsave("./traceplot_example.png", width = 6, height = 4, dpi = 600)

# options(scipen = 1)
# color_scheme_set("pink")
minuspred = bayesplot::ppc_intervals(
  y = samples$z_minus,
  yrep = rstan::extract(fit_g, pars = c("pred_minus"))$pred_minus %>% as.matrix(),
  x = samples$time,
  prob = 0.5
) + xlab("t") + ylab("z-") + 
  scale_x_continuous(breaks = pretty) + scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18))
#ggsave("./GitHub/switching_process/Gillespy2/1.5_1.0_005_001/beta_2_80/zminus_pred.png", width = 10, height = 7, dpi = 600)
minuspred

pluspred = bayesplot::ppc_intervals(
  y = samples$z_plus,
  yrep = rstan::extract(fit, pars = c("pred_plus"))$pred_plus %>% as.matrix(),
  x = samples$time,
  prob = 0.5
) + xlab("t") + ylab("z+") + scale_x_continuous(breaks = pretty) + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18))
#ggsave("./GitHub/switching_process/Gillespy2/1.5_1.0_005_001/beta_2_80/zplus_pred.png", width = 10, height = 7, dpi = 600)
pluspred
minuspred + pluspred
ggsave("./prediction_example.png", dpi = 600, width = 11, height = 4)

posterior_g = as.data.frame(fit_g)
posterior3 = as.data.frame(fit3)
posterior18 = as.data.frame(fit18)
posterior100 = as.data.frame(fit100)
posterior100_ric = as.data.frame(fit100_ric)
posterior100_new = as.data.frame(fit100_new)
posterior100_new_ric = as.data.frame(fit100_new_ric)


posterior_lambda_min = posterior100_new_ric %>% 
  ggplot() + 
  geom_density(aes(x = lambda_minus, y = after_stat(ndensity), color = "Posterior"), linewidth = 1) + 
  #ggtitle("Posterior") + 
  xlim(0,3) + xlab("lambda_minus") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = 1.2, color = "black", linewidth = 1) + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  geom_density(data = prior_lambda, aes(x = gamma, y = after_stat(ndensity), color = "Prior"), linewidth = 1) +
  #stat_function(fun=dgamma, aes(color = "Prior"), args = list(shape = 1200., rate = 1000), linewidth = 1) +
  scale_color_manual(values = c("#00A5AB","#E64823")) 

posterior_lambda_plus = posterior100_new_ric %>% 
  ggplot() + 
  geom_density(aes(x = lambda_plus, y = after_stat(ndensity), color = "Posterior"), linewidth = 1) + 
  #ggtitle("Posterior VS Prior") + 
  xlim(0,3) + xlab("lambda_plus") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = 1.2, color = "black", linewidth = 1) + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  geom_density(data = prior_lambda, aes(x = gamma, y = after_stat(ndensity), color = "Prior"), linewidth = 1) +
  #stat_function(fun=dgamma, aes(color = "Prior"), args = list(shape = 2., rate = 1.5), linewidth = 1) +
  scale_color_manual(values = c("#00A5AB","#E64823")) 

prior_omega_min = data.frame("gamma" = rgamma(10^6,shape=42,rate=5000))

posterior_omega_min = posterior100_new_ric %>% ggplot() + 
  geom_density(aes(x = rate_minus, y = after_stat(ndensity), color = "Posterior"), linewidth = 1) + 
  #ggtitle("Posterior") + 
  xlim(0,0.02) + xlab("ratio_minus") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = 0.01/1.2, color = "black", linewidth = 1) + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  geom_density(data = prior_omega_min, aes(x = gamma, y = after_stat(ndensity), color = "Prior"), linewidth = 1) +
  scale_color_manual(values = c("#00A5AB","#E64823")) 

prior_omega_plus = data.frame("gamma" = rgamma(10^6,shape=4,rate=5000))

posterior_omega_plus = posterior100_new_ric %>% ggplot() + 
  geom_density(aes(x = rate_plus, y = after_stat(ndensity), color = "Posterior"), linewidth = 1) + 
  #ggtitle("Posterior") + 
  xlim(0,0.02) + xlab("ratio_plus") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = 0.001/1.2, color = "black", linewidth = 1) + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  geom_density(data = prior_omega_plus, aes(x = gamma, y = after_stat(ndensity), color = "Prior"), linewidth = 1) +
  scale_color_manual(values = c("#00A5AB","#E64823")) 

posterior_lambda_plus_3 + posterior_lambda_plus_18
(posterior_lambda_min_3 + posterior_lambda_plus_3 +posterior_lambda_min_18 + posterior_lambda_plus_18 )/ ( posterior_omega_min_3 + posterior_omega_plus_3+ posterior_omega_min_18 + posterior_omega_plus_18)
ggsave("./fit18VSfit3.png", dpi = 600, width = 8, height = 8)

posterior_lambda_min / prior_lambda
#ggsave("./GitHub/switching_process/Gillespy2/1.5_1.0_005_001/beta_2_80/lambda_minus_posterior.png", width = 12, height = 7, dpi = 600)
posterior_lambda_plus / prior_lambda
#ggsave("./GitHub/switching_process/Gillespy2/1.5_1.0_005_001/beta_2_80/lambda_plus_posterior.png", width = 12, height = 7, dpi = 600)
posterior_omega_min / prior_omega_p
#ggsave("./GitHub/switching_process/Gillespy2/1.5_1.0_005_001/beta_2_80/omega_minus_posterior.png", width = 12, height = 7, dpi = 600)
posterior_omega_plus / prior_omega

posterior_lambda_min + posterior_lambda_plus + posterior_omega_min + posterior_omega_plus
ggsave("C:/Users/utente/Desktop/UNI/Tesi/Presentation/prior_posterior_fit100_new_ric.png", width = 8, height = 6, dpi = 600)

(posterior_lambda_min + posterior_lambda_plus) / (posterior_omega_plus + posterior_omega_min) 
ggsave("./posterior_example.png", dpi = 600, width = 6, height = 5)

(minuspred + pluspred) / (posterior_lambda_min + posterior_lambda_plus) / (prior_lambda + prior_lambda) / (posterior_omega_min + posterior_omega_plus) / (prior_omega + prior_omega)
ggsave("./GitHub/switching_process/Gillespy2/5t/1.2_1.5_0.01_0.001_5t_51p/gamma_1.5_280/panel_avg.png", width = 12, height = 14, dpi = 600)

# omega posteriors on same plot
fit0 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_0.rds")
fit9 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_9.rds")
fit18 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_18.rds")
fit27 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_27.rds")
fit36 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_36.rds")
fit45 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_45.rds")
fit54 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_54.rds")
fit63 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_63.rds")
fit72 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_72.rds")
fit81 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_81.rds")
fit90 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_90.rds")
fit97 = readRDS("./fit_18/fit_1.2_0.01_0.001/fit_92.rds")
posterior0 = as.data.frame(fit0); posterior9 = as.data.frame(fit9); 
posterior18 = as.data.frame(fit18); posterior27 = as.data.frame(fit27);
posterior36 = as.data.frame(fit36); posterior45 = as.data.frame(fit45);
posterior54 = as.data.frame(fit54); posterior63 = as.data.frame(fit63);
posterior72 = as.data.frame(fit72); posterior81 = as.data.frame(fit81);
posterior90 = as.data.frame(fit90); posterior98 = as.data.frame(fit97)

posterior_rate_plus = ggplot() + 
  geom_density(data = posterior0, aes(x = rate_plus, y = after_stat(density), colour = "0"), linewidth = 0.8) +
  geom_density(data = posterior9, aes(x = rate_plus, y = after_stat(density), colour = "9"), linewidth = .8) +
  geom_density(data = posterior18, aes(x = rate_plus, y = after_stat(density), colour = "18"), linewidth = .8) +
  geom_density(data = posterior27, aes(x = rate_plus, y = after_stat(density), colour = "27"), linewidth = .8) +
  geom_density(data = posterior36, aes(x = rate_plus, y = after_stat(density), colour = "36"), linewidth = .8) +
  geom_density(data = posterior45, aes(x = rate_plus, y = after_stat(density), colour = "45"), linewidth = .8) +
  geom_density(data = posterior54, aes(x = rate_plus, y = after_stat(density), colour = "54"), linewidth = .8) +
  geom_density(data = posterior63, aes(x = rate_plus, y = after_stat(density), colour = "63"), linewidth = .8) +
  geom_density(data = posterior72, aes(x = rate_plus, y = after_stat(density), colour = "72"), linewidth = .8) +
  geom_density(data = posterior81, aes(x = rate_plus, y = after_stat(density), colour = "81"), linewidth = .8) +
  geom_density(data = posterior90, aes(x = rate_plus, y = after_stat(density), colour = "90"), linewidth = .8) +
  geom_density(data = posterior98, aes(x = rate_plus, y = after_stat(density), colour = "98"), linewidth = .8) +
  ggtitle("Posterior ratio_plus") + xlim(0,0.02) + xlab("rate_plus") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_discrete(name = "") +
  # geom_vline(xintercept = 0.001/1.2, color = "black") +
  theme(plot.title = element_text(size = 16)) +
  xlab(expression(hat(omega))) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab(expression(p(hat(omega)))) + 
  # scale_x_continuous(limits = c(0,0.02), breaks = c(0,0.005, 0.01,0.015,0.02)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18), legend.position = "none") +
  geom_vline(xintercept = 0.001/1.2, color = "black", linewidth = 1)

posterior_rate_plus / prior_omega_p


posterior_rate_minus = ggplot() + 
  geom_density(data = posterior0, aes(x = rate_minus, y = after_stat(density), colour = "0"), linewidth = .8) +
  geom_density(data = posterior9, aes(x = rate_minus, y = after_stat(density), colour = "9"), linewidth = .8) +
  geom_density(data = posterior18, aes(x = rate_minus, y = after_stat(density), colour = "18"), linewidth = .8) +
  geom_density(data = posterior27, aes(x = rate_minus, y = after_stat(density), colour = "27"), linewidth = .8) +
  geom_density(data = posterior36, aes(x = rate_minus, y = after_stat(density), colour = "36"), linewidth = .8) +
  geom_density(data = posterior45, aes(x = rate_minus, y = after_stat(density), colour = "45"), linewidth = .8) +
  geom_density(data = posterior54, aes(x = rate_minus, y = after_stat(density), colour = "54"), linewidth = .8) +
  geom_density(data = posterior63, aes(x = rate_minus, y = after_stat(density), colour = "63"), linewidth = .8) +
  geom_density(data = posterior72, aes(x = rate_minus, y = after_stat(density), colour = "72"), linewidth = .8) +
  geom_density(data = posterior81, aes(x = rate_minus, y = after_stat(density), colour = "81"), linewidth = .8) +
  geom_density(data = posterior90, aes(x = rate_minus, y = after_stat(density), colour = "90"), linewidth = .8) +
  geom_density(data = posterior98, aes(x = rate_minus, y = after_stat(density), colour = "98"), linewidth = .8) +
  ggtitle("Posterior ratio_plus") + xlim(0,0.02) + xlab("rate_minus") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_discrete(name = "") +
  # geom_vline(xintercept = 0.001/1.2, color = "black") +
  theme(plot.title = element_text(size = 16)) +
  xlab(expression(hat(omega))) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab(expression(p(hat(omega)))) + 
  # scale_x_continuous(limits = c(0,0.02), breaks = c(0,0.005, 0.01,0.015,0.02)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18), legend.position = "none") +
  geom_vline(xintercept = 0.01/1.2, color = "black", linewidth = 1)

(posterior_rate_minus + posterior_rate_plus) / (prior_omega + prior_omega_p)


ggsave("./posteriors_1.2_0.01_0.001.png", width = 10, height = 8, dpi = 600)

posterior_lambda_plus = ggplot() + 
  geom_density(data = posterior0, aes(x = lambda_plus, y = after_stat(density), colour = "0"), linewidth = 0.8) +
  geom_density(data = posterior9, aes(x = lambda_plus, y = after_stat(density), colour = "9"), linewidth = .8) +
  geom_density(data = posterior18, aes(x = lambda_plus, y = after_stat(density), colour = "18"), linewidth = .8) +
  geom_density(data = posterior27, aes(x = lambda_plus, y = after_stat(density), colour = "27"), linewidth = .8) +
  geom_density(data = posterior36, aes(x = lambda_plus, y = after_stat(density), colour = "36"), linewidth = .8) +
  geom_density(data = posterior45, aes(x = lambda_plus, y = after_stat(density), colour = "45"), linewidth = .8) +
  geom_density(data = posterior54, aes(x = lambda_plus, y = after_stat(density), colour = "54"), linewidth = .8) +
  geom_density(data = posterior63, aes(x = lambda_plus, y = after_stat(density), colour = "63"), linewidth = .8) +
  geom_density(data = posterior72, aes(x = lambda_plus, y = after_stat(density), colour = "72"), linewidth = .8) +
  geom_density(data = posterior81, aes(x = lambda_plus, y = after_stat(density), colour = "81"), linewidth = .8) +
  geom_density(data = posterior90, aes(x = lambda_plus, y = after_stat(density), colour = "90"), linewidth = .8) +
  geom_density(data = posterior99, aes(x = lambda_plus, y = after_stat(density), colour = "99"), linewidth = .8) +
  ggtitle("Posterior lambda_plus") + xlim(0,3) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_discrete(name = "") +
  # geom_vline(xintercept = 0.001/1.2, color = "black") +
  theme(plot.title = element_text(size = 16)) +
  xlab(expression(lambda)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab(expression(p(lambda))) + 
  # scale_x_continuous(limits = c(0,0.02), breaks = c(0,0.005, 0.01,0.015,0.02)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18), legend.position = "none") +
  geom_vline(xintercept = 1.2, color = "black", linewidth = 1)

posterior_lambda_plus 


posterior_lambda_minus = ggplot() + 
  geom_density(data = posterior0, aes(x = lambda_minus, y = after_stat(density), colour = "0"), linewidth = .8) +
  geom_density(data = posterior9, aes(x = lambda_minus, y = after_stat(density), colour = "9"), linewidth = .8) +
  geom_density(data = posterior18, aes(x = lambda_minus, y = after_stat(density), colour = "18"), linewidth = .8) +
  geom_density(data = posterior27, aes(x = lambda_minus, y = after_stat(density), colour = "27"), linewidth = .8) +
  geom_density(data = posterior36, aes(x = lambda_minus, y = after_stat(density), colour = "36"), linewidth = .8) +
  geom_density(data = posterior45, aes(x = lambda_minus, y = after_stat(density), colour = "45"), linewidth = .8) +
  geom_density(data = posterior54, aes(x = lambda_minus, y = after_stat(density), colour = "54"), linewidth = .8) +
  geom_density(data = posterior63, aes(x = lambda_minus, y = after_stat(density), colour = "63"), linewidth = .8) +
  geom_density(data = posterior72, aes(x = lambda_minus, y = after_stat(density), colour = "72"), linewidth = .8) +
  geom_density(data = posterior81, aes(x = lambda_minus, y = after_stat(density), colour = "81"), linewidth = .8) +
  geom_density(data = posterior90, aes(x = lambda_minus, y = after_stat(density), colour = "90"), linewidth = .8) +
  geom_density(data = posterior99, aes(x = lambda_minus, y = after_stat(density), colour = "99"), linewidth = .8) +
  ggtitle("Posterior lambda_minus") + xlim(0,3) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_discrete(name = "") +
  # geom_vline(xintercept = 0.001/1.2, color = "black") +
  theme(plot.title = element_text(size = 16)) +
  xlab(expression(lambda)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab(expression(p(lambda))) + 
  # scale_x_continuous(limits = c(0,0.02), breaks = c(0,0.005, 0.01,0.015,0.02)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18), legend.position = "none") +
  geom_vline(xintercept = 1.2, color = "black", linewidth = 1)

posterior_lambda_minus / posterior_lambda_plus /  prior_lambda
ggsave("./posterior_lambda_1.2_0.01_0.001.png", dpi = 600, width = 4, height = 8)

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
