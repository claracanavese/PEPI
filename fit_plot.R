
# fit 3
lm = 0; lp = 0; rm = 0; rp = 0;
for (i in seq(0,99)) {
  fit = readRDS(paste0("./fit_3/fit_1.0_1.5_0.005/fit_",i,".rds"))
  lm = lm + mean(unlist(rstan::extract(fit, pars = c("lambda_minus"))))
  lp = lp + mean(unlist(rstan::extract(fit, pars = c("lambda_plus"))))
  rm = rm + mean(unlist(rstan::extract(fit, pars = c("rate_minus"))))
  rp = rp + mean(unlist(rstan::extract(fit, pars = c("rate_plus"))))
  print(i)
}

err_lm = abs(1. - lm/100)/1.
err_lp = abs(1.5 - lp/100)/1.5
err_rm = abs(0.005/1.5 - rm/100)/(0.005/1.5)
err_rp = abs(0.005/1 - rp/100)/(0.005/1)

err3_1 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.01_0.001 ok
err3_2 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.005 ok
err3_3 = c(err_lm, err_lp, err_rm, err_rp) # 1.5_1.0_0.001_0.01 NO

err3_4 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.01_0.001 ok
err3_5 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.005 ni
err3_6 = c(err_lm, err_lp, err_rm, err_rp) # 1.2_0.001_0.01 NO

err3_7 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.01_0.001 ok
err3_8 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.005
err3_9 = c(err_lm, err_lp, err_rm, err_rp) # 1.0_1.5_0.001_0.01

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
colnames(err3) = c("lm","lp","om","op")

err3 = err3 %>% mutate(ID = 3)

err = data.frame()
err = rbind(err,err3)
err = rbind(err,err8)
err = rbind(err,err11)
err = rbind(err,err15)

err = err %>% mutate(lm = lm*100, lp = lp*100, om = om*100, op = op*100)

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
