
# fit 3
lm = 0; lp = 0; rm = 0; rp = 0;
for (i in seq(0,80)) {
  fit = readRDS(paste0("./fit_3/fit_1.2_0.001_0.01/fit_",i,".rds"))
  lm = lm + mean(unlist(rstan::extract(fit, pars = c("lambda_minus"))))
  lp = lp + mean(unlist(rstan::extract(fit, pars = c("lambda_plus"))))
  rm = rm + mean(unlist(rstan::extract(fit, pars = c("rate_minus"))))
  rp = rp + mean(unlist(rstan::extract(fit, pars = c("rate_plus"))))
  print(i)
}

err_lm = abs(1.5 - lm/81)/1.
err_lp = abs(1. - lp/81)/1.
err_rm = abs(0.001/1. - rm/81)/(0.001/1.)
err_rp = abs(0.01/1.5 - rp/81)/(0.01/1.5)

err3_1 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.01_0.001 ok
err3_2 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.005 ok
err3_3 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.001_0.01 81fits

err3_4 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.01_0.001 ok
err3_5 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.005 ni
err3_6 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.001_0.01 NO

err3_7 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.01_0.001 ok
err3_8 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.005 ok
err3_9 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.001_0.01 NO

err3 = data.frame()
err3 = rbind(err3,err3_1)
err3 = rbind(err3,err3_2)
err3 = rbind(err3,err3_3)
err3 = rbind(err3,err3_4)
err3 = rbind(err3,err3_5)
err3 = rbind(err3,err3_6)
err3 = rbind(err3,err3_7)
err3 = rbind(err3,err3_8)
err3 = rbind(err3,err3_9)
colnames(err3) = c("lm","lp","rm","rp")
saveRDS(err3,"./err3.rds")

err3 = err3 %>% mutate(ID = 3)

# fit 8
lm = 0; lp = 0; rm = 0; rp = 0;
for (i in seq(0,99)) {
  fit = readRDS(paste0("./fit_8/fit_1.0_1.5_0.001_0.01/fit_",i,".rds"))
  lm = lm + mean(unlist(rstan::extract(fit, pars = c("lambda_minus"))))
  lp = lp + mean(unlist(rstan::extract(fit, pars = c("lambda_plus"))))
  rm = rm + mean(unlist(rstan::extract(fit, pars = c("rate_minus"))))
  rp = rp + mean(unlist(rstan::extract(fit, pars = c("rate_plus"))))
  print(i)
}

err_lm = abs(1. - lm/100)/1.
err_lp = abs(1.5 - lp/100)/1.5
err_rm = abs(0.001/1.5 - rm/100)/(0.001/1.5)
err_rp = abs(0.01/1. - rp/100)/(0.01/1.)

err8_1 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.01_0.001 ok
err8_2 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.005 ok
err8_3 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.001_0.01 NO

err8_4 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.01_0.001 ok
err8_5 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.005 ni
err8_6 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.001_0.01 no

err8_7 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.01_0.001 ok
err8_8 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.005 ok
err8_9 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.001_0.01 no

err8 = data.frame()
err8 = rbind(err8,err8_1)
err8 = rbind(err8,err8_2)
err8 = rbind(err8,err8_3)
err8 = rbind(err8,err8_4)
err8 = rbind(err8,err8_5)
err8 = rbind(err8,err8_6)
err8 = rbind(err8,err8_7)
err8 = rbind(err8,err8_8)
err8 = rbind(err8,err8_9)
colnames(err8) = c("lm","lp","rm","rp")

err8 = err8 %>% mutate(ID = 8)

# fit 13
lm = 0; lp = 0; rm = 0; rp = 0;
for (i in seq(0,97)) {
  fit = readRDS(paste0("./fit_13/fit_1.5_1.0_0.01_0.001/fit_",i,".rds"))
  lm = lm + mean(unlist(rstan::extract(fit, pars = c("lambda_minus"))))
  lp = lp + mean(unlist(rstan::extract(fit, pars = c("lambda_plus"))))
  rm = rm + mean(unlist(rstan::extract(fit, pars = c("rate_minus"))))
  rp = rp + mean(unlist(rstan::extract(fit, pars = c("rate_plus"))))
  print(i)
}

err_lm = abs(1.5 - lm/98)/1.5
err_lp = abs(1. - lp/98)/1.
err_rm = abs(0.01/1. - rm/98)/(0.01/1.)
err_rp = abs(0.001/1.5 - rp/98)/(0.001/1.5)

err13_1 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.01_0.001 
err13_2 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.005 
err13_3 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.001_0.01 

err13_4 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.01_0.001 
err13_5 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.005 
err13_6 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.001_0.01 

err13_7 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.01_0.001 
err13_8 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.005 
err13_9 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.001_0.01 

err13 = data.frame()
err13 = rbind(err13, err13_1)
err13 = rbind(err13, err13_2)
err13 = rbind(err13, err13_3)
err13 = rbind(err13, err13_4)
err13 = rbind(err13, err13_5)
err13 = rbind(err13, err13_6)
err13 = rbind(err13, err13_7)
err13 = rbind(err13, err13_8)
err13 = rbind(err13, err13_9)
colnames(err13) = c("lm","lp","rm","rp")

err13= err13 %>% mutate(ID = 13)

# fit 18
lm = 0; lp = 0; rm = 0; rp = 0;
for (i in seq(0,97)) {
  fit = readRDS(paste0("./fit_18/fit_1.5_1.0_0.01_0.001/fit_",i,".rds"))
  lm = lm + mean(unlist(rstan::extract(fit, pars = c("lambda_minus"))))
  lp = lp + mean(unlist(rstan::extract(fit, pars = c("lambda_plus"))))
  rm = rm + mean(unlist(rstan::extract(fit, pars = c("rate_minus"))))
  rp = rp + mean(unlist(rstan::extract(fit, pars = c("rate_plus"))))
  print(i)
}

err_lm = abs(1.5 - lm/98)/1.5
err_lp = abs(1.0 - lp/98)/1.
err_rm = abs(0.01/1. - rm/98)/(0.01/1.)
err_rp = abs(0.001/1.5 - rp/98)/(0.001/1.5)

err18_1 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.01_0.001 
err18_2 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.005 
err18_3 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.001_0.01 

err18_4 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.01_0.001 
err18_5 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.005 
err18_6 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.001_0.01 

err18_7 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.01_0.001 
err18_8 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.005 
err18_9 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.001_0.01 

err18 = data.frame()
err18 = rbind(err18, err18_1)
err18 = rbind(err18, err18_2)
err18 = rbind(err18, err18_3)
err18 = rbind(err18, err18_4)
err18 = rbind(err18, err18_5)
err18 = rbind(err18, err18_6)
err18 = rbind(err18, err18_7)
err18 = rbind(err18, err18_8)
err18 = rbind(err18, err18_9)
colnames(err18) = c("lm","lp","rm","rp")

err18= err18 %>% mutate(ID = 18)

# 1.5_1.0_0.001_0.01
err_8

# complete error dataframe
err = data.frame()
err = rbind(err,err3)
err = rbind(err,err8)
err = rbind(err,err13)
err = rbind(err,err18)

err = err %>% mutate(lm = lm*100, lp = lp*100, rm = rm*100, rp = rp*100)

err %>% ggplot() +
  geom_point(aes(x = ID, y = lm, color = "lm")) +
  geom_point(aes(x = ID, y = lp, color = "lp")) +
  geom_point(aes(x = ID, y = rm, color = "rm")) +
  geom_point(aes(x = ID, y = rp, color = "rp")) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "% error") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,100)
ggsave("./plot_1.5_1.0_0.01_0.001_inference.png",dpi=600, width=6,height=5)

plot1 = err[c(1,10,19,28),] %>% ggplot() +
  geom_point(aes(x = ID, y = lm, color = "lm")) +
  geom_point(aes(x = ID, y = lp, color = "lp")) +
  geom_point(aes(x = ID, y = om, color = "om")) +
  geom_point(aes(x = ID, y = op, color = "op")) +
  scale_x_continuous(breaks = c(3,8,11,15)) + labs(color = NULL, x = "# obs", y = "% error")

plot2 = err[c(2,11,20,29),] %>% ggplot() +
  geom_point(aes(x = ID, y = lm, color = "lm")) +
  geom_point(aes(x = ID, y = lp, color = "lp")) +
  geom_point(aes(x = ID, y = om, color = "om")) +
  geom_point(aes(x = ID, y = op, color = "op")) +
  scale_x_continuous(breaks = c(3,8,11,15)) + labs(color = NULL, x = "# obs", y = "% error")

plot3 = err[c(3,12,21,30),] %>% ggplot() +
  geom_point(aes(x = ID, y = lm, color = "lm")) +
  geom_point(aes(x = ID, y = lp, color = "lp")) +
  geom_point(aes(x = ID, y = om, color = "om")) +
  geom_point(aes(x = ID, y = op, color = "op")) +
  scale_x_continuous(breaks = c(3,8,11,15)) + labs(color = NULL, x = "# obs", y = "% error")

plot4 = err[c(4,13,22,31),] %>% ggplot() +
  geom_point(aes(x = ID, y = lm, color = "lm")) +
  geom_point(aes(x = ID, y = lp, color = "lp")) +
  geom_point(aes(x = ID, y = om, color = "om")) +
  geom_point(aes(x = ID, y = op, color = "op")) +
  scale_x_continuous(breaks = c(3,8,11,15)) + labs(color = NULL, x = "# obs", y = "% error")

plot5 = err[c(5,14,23,32),] %>% ggplot() +
  geom_point(aes(x = ID, y = lm, color = "lm")) +
  geom_point(aes(x = ID, y = lp, color = "lp")) +
  geom_point(aes(x = ID, y = om, color = "om")) +
  geom_point(aes(x = ID, y = op, color = "op")) +
  scale_x_continuous(breaks = c(3,8,11,15)) + labs(color = NULL, x = "# obs", y = "% error")

plot6= err[c(6,15,24,33),] %>% ggplot() +
  geom_point(aes(x = ID, y = lm, color = "lm")) +
  geom_point(aes(x = ID, y = lp, color = "lp")) +
  geom_point(aes(x = ID, y = om, color = "om")) +
  geom_point(aes(x = ID, y = op, color = "op")) +
  scale_x_continuous(breaks = c(3,8,11,15)) + labs(color = NULL, x = "# obs", y = "% error")

plot7 = err[c(7,16,25,34),] %>% ggplot() +
  geom_point(aes(x = ID, y = lm, color = "lm")) +
  geom_point(aes(x = ID, y = lp, color = "lp")) +
  geom_point(aes(x = ID, y = om, color = "om")) +
  geom_point(aes(x = ID, y = op, color = "op")) +
  scale_x_continuous(breaks = c(3,8,11,15)) + labs(color = NULL, x = "# obs", y = "% error")

plot8 = err[c(8,17,26,35),] %>% ggplot() +
  geom_point(aes(x = ID, y = lm, color = "lm")) +
  geom_point(aes(x = ID, y = lp, color = "lp")) +
  geom_point(aes(x = ID, y = om, color = "om")) +
  geom_point(aes(x = ID, y = op, color = "op")) +
  scale_x_continuous(breaks = c(3,8,11,15)) + labs(color = NULL, x = "# obs", y = "% error")

plot9 = err[c(9,18,27,36),] %>% ggplot() +
  geom_point(aes(x = ID, y = lm, color = "lm")) +
  geom_point(aes(x = ID, y = lp, color = "lp")) +
  geom_point(aes(x = ID, y = om, color = "om")) +
  geom_point(aes(x = ID, y = op, color = "op")) +
  scale_x_continuous(breaks = c(3,8,11,15)) + labs(color = NULL, x = "# obs", y = "% error")

(plot1 + plot2 + plot3) / (plot4 + plot5 + plot6) / (plot7 + plot8 + plot9)
ggsave("./inference_error.png", width = 10, height = 8, dpi = 600)

fit = readRDS("./fit_11/fit_37.rds")
print(fit, pars = c("lambda_minus", "lambda_plus", "omega_minus", "omega_plus"), digits_summary = 4)
posterior = as.data.frame(fit)
posterior_lambda_min = posterior %>% ggplot() + geom_density(aes(x = lambda_minus, y = after_stat(density))) + ggtitle("Posterior") + xlim(0,5) + xlab("lambda_minus") + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = 1.5, color = "forestgreen")
posterior_lambda_plus = posterior %>% ggplot() + geom_density(aes(x = lambda_plus, y = after_stat(density))) + ggtitle("Posterior") + xlim(0,5) + xlab("lambda_plus") + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = 1.0, color = "forestgreen")
posterior_omega_min = posterior %>% ggplot() + geom_density(aes(x = omega_minus, y = after_stat(density))) + ggtitle("Posterior") + xlim(0,0.03) + xlab("omega_minus") + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = 0.001, color = "forestgreen")
posterior_omega_plus = posterior %>% ggplot() + geom_density(aes(x = omega_plus, y = after_stat(density))) + ggtitle("Posterior") + xlim(0,0.03) + xlab("omega_plus") + theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = 0.01, color = "forestgreen")

prior_omega = 
  ggplot() + stat_function(fun = dgamma, args = list(shape = 1.5, rate = 200)) +
  ggtitle("Prior") + xlab("omega") + theme(plot.title = element_text(hjust = 0.5)) + ylab("density") + xlim(0.,0.03)

posterior_omega_min / prior_omega
