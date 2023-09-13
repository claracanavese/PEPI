
lm = 0; lp = 0; om = 0; op = 0;
for (i in seq(9,900, by = 9)) {
  fit = readRDS(paste0("./fit_15/fit_",i,".rds"))
  lm = lm + mean(unlist(rstan::extract(fit, pars = c("lambda_minus"))))
  lp = lp + mean(unlist(rstan::extract(fit, pars = c("lambda_plus"))))
  om = om + mean(unlist(rstan::extract(fit, pars = c("omega_minus"))))
  op = op + mean(unlist(rstan::extract(fit, pars = c("omega_plus"))))
  print(i)
}

err_lm = abs(1.2 - lm/100)/1.2
err_lp = abs(1.2 - lp/100)/1.2
err_om = abs(0.005 - om/100)/0.005
err_op = abs(0.005 - op/100)/0.005

err15_9 = c(err_lm, err_lp, err_om, err_op)
err15_8 = c(err_lm, err_lp, err_om, err_op)
err15_7 = c(err_lm, err_lp, err_om, err_op)
err15_6 = c(err_lm, err_lp, err_om, err_op)
err15_5 = c(err_lm, err_lp, err_om, err_op)
err15_4 = c(err_lm, err_lp, err_om, err_op)
err15_3 = c(err_lm, err_lp, err_om, err_op)
err15_2 = c(err_lm, err_lp, err_om, err_op)
err15_1 = c(err_lm, err_lp, err_om, err_op)

err15 = data.frame()
err15 = rbind(err15,err15_9)
colnames(err15) = c("lm","lp","om","op")

err15 = err15 %>% mutate(ID = 15)

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
