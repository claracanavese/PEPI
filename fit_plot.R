
# fit 3
err3_1.5_1.0_0.01_0.001 = data.frame()
err3_1.5_1.0_0.005 = data.frame()
err3_1.5_1.0_0.001_0.01 = data.frame()

err3_1.2_0.01_0.001 = data.frame()
err3_1.2_0.005 = data.frame()
err3_1.2_0.001_0.01 = data.frame()

err3_1.0_1.5_0.01_0.001 = data.frame()
err3_1.0_1.5_0.005 = data.frame()
err3_1.0_1.5_0.001_0.01 = data.frame()

saveRDS(err3_1.5_1.0_0.01_0.001,"./err3_1.5_1.0_0.01_0.001.rds")
saveRDS(err3_1.5_1.0_0.005,"./err3_1.5_1.0_0.005.rds")
saveRDS(err3_1.5_1.0_0.001_0.01,"./err3_1.5_1.0_0.001_0.01.rds")

saveRDS(err3_1.2_0.01_0.001,"./err3_1.2_0.01_0.001.rds")
saveRDS(err3_1.2_0.005,"./err3_1.2_0.005.rds")
saveRDS(err3_1.2_0.001_0.01,"./err3_1.2_0.001_0.01.rds")

saveRDS(err3_1.0_1.5_0.01_0.001,"./err3_1.0_1.5_0.01_0.001.rds")
saveRDS(err3_1.0_1.5_0.005,"./err3_1.0_1.5_0.005.rds")
saveRDS(err3_1.0_1.5_0.001_0.01,"./err3_1.0_1.5_0.001_0.01.rds")

lm = 0; lp = 0; rm = 0; rp = 0;
for (i in seq(0,99)) {
  fit = readRDS(paste0("./fit_3/fit_1.0_1.5_0.001_0.01/fit_",i,".rds"))
  lm = mean(unlist(rstan::extract(fit, pars = c("lambda_minus"))))
  lp = mean(unlist(rstan::extract(fit, pars = c("lambda_plus"))))
  rm = mean(unlist(rstan::extract(fit, pars = c("rate_minus"))))
  rp = mean(unlist(rstan::extract(fit, pars = c("rate_plus"))))
  err3_1.0_1.5_0.001_0.01 = rbind(err3_1.0_1.5_0.001_0.01,c(lm,lp,rm,rp))
  print(i)
}
colnames(err3_1.0_1.5_0.001_0.01) = c("lm","lp","rm","rp")

err3_1.5_1.0_0.005 = readRDS("./err3_1.5_1.0_0.005.rds")
err3 = err3 %>% mutate(ID = 3)

# fit 8
err8_1.5_1.0_0.01_0.001 = data.frame()
err8_1.5_1.0_0.005 = data.frame()
err8_1.5_1.0_0.001_0.01 = data.frame()

err8_1.2_0.01_0.001 = data.frame()
err8_1.2_0.005 = data.frame()
err8_1.2_0.001_0.01 = data.frame()

err8_1.0_1.5_0.01_0.001 = data.frame()
err8_1.0_1.5_0.005 = data.frame()
err8_1.0_1.5_0.001_0.01 = data.frame()

lm = 0; lp = 0; rm = 0; rp = 0;
for (i in seq(0,99)) {
  fit = readRDS(paste0("./fit_8/fit_1.2_0.001_0.01/fit_",i,".rds"))
  lm = mean(unlist(rstan::extract(fit, pars = c("lambda_minus"))))
  lp = mean(unlist(rstan::extract(fit, pars = c("lambda_plus"))))
  rm = mean(unlist(rstan::extract(fit, pars = c("rate_minus"))))
  rp = mean(unlist(rstan::extract(fit, pars = c("rate_plus"))))
  err8_1.2_0.001_0.01 = rbind(err8_1.2_0.001_0.01,c(lm,lp,rm,rp))
  print(i)
}
colnames(err8_1.2_0.001_0.01) = c("lm","lp","rm","rp")

saveRDS(err8_1.5_1.0_0.01_0.001,"./err8_1.5_1.0_0.01_0.001.rds")
saveRDS(err8_1.5_1.0_0.005,"./err8_1.5_1.0_0.005.rds")
saveRDS(err8_1.5_1.0_0.001_0.01,"./err8_1.5_1.0_0.001_0.01.rds")

saveRDS(err8_1.2_0.01_0.001,"./err8_1.2_0.01_0.001.rds")
saveRDS(err8_1.2_0.005,"./err8_1.2_0.005.rds")
saveRDS(err8_1.2_0.001_0.01,"./err8_1.2_0.001_0.01.rds")

saveRDS(err8_1.0_1.5_0.01_0.001,"./err8_1.0_1.5_0.01_0.001.rds")
saveRDS(err8_1.0_1.5_0.005,"./err8_1.0_1.5_0.005.rds")
saveRDS(err8_1.0_1.5_0.001_0.01,"./err8_1.0_1.5_0.001_0.01.rds")

err8 = err8 %>% mutate(ID = 8)

# fit 13
err13_1.5_1.0_0.01_0.001 = data.frame()
err13_1.5_1.0_0.005 = data.frame()
err13_1.5_1.0_0.001_0.01 = data.frame()

err13_1.2_0.01_0.001 = data.frame()
err13_1.2_0.005 = data.frame()
err13_1.2_0.001_0.01 = data.frame()

err13_1.0_1.5_0.01_0.001 = data.frame()
err13_1.0_1.5_0.005 = data.frame()
err13_1.0_1.5_0.001_0.01 = data.frame()

lm = 0; lp = 0; rm = 0; rp = 0;
for (i in seq(0,99)) {
  fit = readRDS(paste0("./fit_13/fit_1.0_1.5_0.001_0.01/fit_",i,".rds"))
  lm = mean(unlist(rstan::extract(fit, pars = c("lambda_minus"))))
  lp = mean(unlist(rstan::extract(fit, pars = c("lambda_plus"))))
  rm = mean(unlist(rstan::extract(fit, pars = c("rate_minus"))))
  rp = mean(unlist(rstan::extract(fit, pars = c("rate_plus"))))
  err13_1.0_1.5_0.001_0.01 = rbind(err13_1.0_1.5_0.001_0.01,c(lm,lp,rm,rp))
  print(i)
}
colnames(err13_1.0_1.5_0.001_0.01) = c("lm","lp","rm","rp")

saveRDS(err13_1.5_1.0_0.01_0.001,"./err13_1.5_1.0_0.01_0.001.rds")
saveRDS(err13_1.5_1.0_0.005,"./err13_1.5_1.0_0.005.rds")
saveRDS(err13_1.5_1.0_0.001_0.01,"./err13_1.5_1.0_0.001_0.01.rds")

saveRDS(err13_1.2_0.01_0.001,"./err13_1.2_0.01_0.001.rds")
saveRDS(err13_1.2_0.005,"./err13_1.2_0.005.rds")
saveRDS(err13_1.2_0.001_0.01,"./err13_1.2_0.001_0.01.rds")

saveRDS(err13_1.0_1.5_0.01_0.001,"./err13_1.0_1.5_0.01_0.001.rds")
saveRDS(err13_1.0_1.5_0.005,"./err13_1.0_1.5_0.005.rds")
saveRDS(err13_1.0_1.5_0.001_0.01,"./err13_1.0_1.5_0.001_0.01.rds")

err13= err13 %>% mutate(ID = 13)

# fit 18
err18_1.5_1.0_0.01_0.001 = data.frame()
err18_1.5_1.0_0.005 = data.frame()
err18_1.5_1.0_0.001_0.01 = data.frame()

err18_1.2_0.01_0.001 = data.frame()
err18_1.2_0.005 = data.frame()
err18_1.2_0.001_0.01 = data.frame()

err18_1.0_1.5_0.01_0.001 = data.frame()
err18_1.0_1.5_0.005 = data.frame()
err18_1.0_1.5_0.001_0.01 = data.frame()

lm = 0; lp = 0; rm = 0; rp = 0;
for (i in seq(62,99)) {
  fit = readRDS(paste0("./fit_18/fit_1.0_1.5_0.005/fit_",i,".rds"))
  lm = mean(unlist(rstan::extract(fit, pars = c("lambda_minus"))))
  lp = mean(unlist(rstan::extract(fit, pars = c("lambda_plus"))))
  rm = mean(unlist(rstan::extract(fit, pars = c("rate_minus"))))
  rp = mean(unlist(rstan::extract(fit, pars = c("rate_plus"))))
  err18_1.0_1.5_0.005 = rbind(err18_1.0_1.5_0.005,c(lm,lp,rm,rp))
  print(i)
}
colnames(err18_1.0_1.5_0.005) = c("lm","lp","rm","rp")

saveRDS(err18_1.5_1.0_0.01_0.001,"./err18_1.5_1.0_0.01_0.001.rds")
saveRDS(err18_1.5_1.0_0.005,"./err18_1.5_1.0_0.005.rds")
saveRDS(err18_1.5_1.0_0.001_0.01,"./err18_1.5_1.0_0.001_0.01.rds")

saveRDS(err18_1.2_0.01_0.001,"./err18_1.2_0.01_0.001.rds")
saveRDS(err18_1.2_0.005,"./err18_1.2_0.005.rds")
saveRDS(err18_1.2_0.001_0.01,"./err18_1.2_0.001_0.01.rds")

saveRDS(err18_1.0_1.5_0.01_0.001,"./err18_1.0_1.5_0.01_0.001.rds")
saveRDS(err18_1.0_1.5_0.005,"./err18_1.0_1.5_0.005.rds")
saveRDS(err18_1.0_1.5_0.001_0.01,"./err18_1.0_1.5_0.001_0.01.rds")


# complete error dataframe
# case 1 A
err_1.5_1.0_0.01_0.001 = data.frame()
err_1.5_1.0_0.01_0.001 = rbind(err_1.5_1.0_0.01_0.001, c(mean(err3_1.5_1.0_0.01_0.001$lm),
                                                         mean(err3_1.5_1.0_0.01_0.001$lp),
                                                         mean(err3_1.5_1.0_0.01_0.001$rm),
                                                         mean(err3_1.5_1.0_0.01_0.001$rp),
                                                         sd(err3_1.5_1.0_0.01_0.001$lm),
                                                         sd(err3_1.5_1.0_0.01_0.001$lp),
                                                         sd(err3_1.5_1.0_0.01_0.001$rm),
                                                         sd(err3_1.5_1.0_0.01_0.001$rp),
                                                         3))
err_1.5_1.0_0.01_0.001 = rbind(err_1.5_1.0_0.01_0.001, c(mean(err8_1.5_1.0_0.01_0.001$lm),
                                                         mean(err8_1.5_1.0_0.01_0.001$lp),
                                                         mean(err8_1.5_1.0_0.01_0.001$rm),
                                                         mean(err8_1.5_1.0_0.01_0.001$rp),
                                                         sd(err8_1.5_1.0_0.01_0.001$lm),
                                                         sd(err8_1.5_1.0_0.01_0.001$lp),
                                                         sd(err8_1.5_1.0_0.01_0.001$rm),
                                                         sd(err8_1.5_1.0_0.01_0.001$rp),
                                                         8))
err_1.5_1.0_0.01_0.001 = rbind(err_1.5_1.0_0.01_0.001, c(mean(err13_1.5_1.0_0.01_0.001$lm),
                                                         mean(err13_1.5_1.0_0.01_0.001$lp),
                                                         mean(err13_1.5_1.0_0.01_0.001$rm),
                                                         mean(err13_1.5_1.0_0.01_0.001$rp),
                                                         sd(err13_1.5_1.0_0.01_0.001$lm),
                                                         sd(err13_1.5_1.0_0.01_0.001$lp),
                                                         sd(err13_1.5_1.0_0.01_0.001$rm),
                                                         sd(err13_1.5_1.0_0.01_0.001$rp),
                                                         13))
err_1.5_1.0_0.01_0.001 = rbind(err_1.5_1.0_0.01_0.001, c(mean(err18_1.5_1.0_0.01_0.001$lm),
                                                         mean(err18_1.5_1.0_0.01_0.001$lp),
                                                         mean(err18_1.5_1.0_0.01_0.001$rm),
                                                         mean(err18_1.5_1.0_0.01_0.001$rp),
                                                         sd(err18_1.5_1.0_0.01_0.001$lm),
                                                         sd(err18_1.5_1.0_0.01_0.001$lp),
                                                         sd(err18_1.5_1.0_0.01_0.001$rm),
                                                         sd(err18_1.5_1.0_0.01_0.001$rp),
                                                         18))

colnames(err_1.5_1.0_0.01_0.001) = c("lm","lp","rm","rp","slm","slp","srm","srp","obs")

# growth rates
err_1.5_1.0_0.01_0.001 %>% 
  ggplot() +
  geom_point(aes(x = obs, y = lm, color = "lm")) +
  geom_errorbar(aes(x = obs, ymin=lm-slm, ymax=lm+slm), width=.2) +
  geom_point(aes(x = obs, y = lp, color = "lp")) +
  geom_errorbar(aes(x = obs, ymin=lp-slp, ymax=lp+slp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "parameter") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,2)

# switching rates
err_1.5_1.0_0.01_0.001 %>% 
  ggplot() +
  geom_point(aes(x = obs, y = rm, color = "rm")) +
  geom_errorbar(aes(x = obs, ymin=rm-srm, ymax=rm+srm), width=.2) +
  geom_point(aes(x = obs, y = rp, color = "rp")) +
  geom_errorbar(aes(x = obs, ymin=rp-srp, ymax=rp+srp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "parameter") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,0.015)

# % errors
plot1a = err_1.5_1.0_0.01_0.001 %>% 
  mutate(err_lm = abs(1.5-lm)/1.5*100,
         err_lp = abs(1.-lp)/1.*100,
         err_rm = abs(0.01-rm)/(0.01)*100,
         err_rp = abs(0.001/1.5-rp)/0.001/(1.5)*100) %>% 
  ggplot() +
  geom_jitter(aes(x = obs, y = err_lm, color = "lamba_minus"),size=2.5, width = 0.3) +
  # geom_errorbar(aes(x = obs, ymin=err_lm-slm, ymax=err_lm+slm), width=.2) +
  geom_jitter(aes(x = obs, y = err_lp, color = "lamda_plus"),size=2.5,width = 0.3) +
  # geom_errorbar(aes(x = obs, ymin=err_lp-slp, ymax=err_lp+slp), width=.2) +
  geom_jitter(aes(x = obs, y = err_rm, color = "rate_minus"),size=2.5,width = 0.3) +
  # geom_errorbar(aes(x = obs, ymin=err_rm-srm, ymax=err_rm+srm), width=.2) +
  geom_jitter(aes(x = obs, y = err_rp, color = "rate_plus"),size=2.5, width = 0.3) +
  # geom_errorbar(aes(x = obs, ymin=err_rp-srp, ymax=err_rp+srp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "% error") +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18), legend.position = "none") +
  ylim(0,100)
ggsave("./last_plot.png",dpi = 600,width = 5,height = 4)


# case 1 B
err_1.5_1.0_0.005 = data.frame()
err_1.5_1.0_0.005 = rbind(err_1.5_1.0_0.005, c(mean(err3_1.5_1.0_0.005$lm),
                                                mean(err3_1.5_1.0_0.005$lp),
                                                mean(err3_1.5_1.0_0.005$rm),
                                                mean(err3_1.5_1.0_0.005$rp),
                                                sd(err3_1.5_1.0_0.005$lm),
                                                sd(err3_1.5_1.0_0.005$lp),
                                                sd(err3_1.5_1.0_0.005$rm),
                                                sd(err3_1.5_1.0_0.005$rp),
                                                3))
err_1.5_1.0_0.005 = rbind(err_1.5_1.0_0.005, c(mean(err8_1.5_1.0_0.005$lm),
                                               mean(err8_1.5_1.0_0.005$lp),
                                               mean(err8_1.5_1.0_0.005$rm),
                                               mean(err8_1.5_1.0_0.005$rp),
                                               sd(err8_1.5_1.0_0.005$lm),
                                               sd(err8_1.5_1.0_0.005$lp),
                                               sd(err8_1.5_1.0_0.005$rm),
                                               sd(err8_1.5_1.0_0.005$rp),
                                               8))
err_1.5_1.0_0.005 = rbind(err_1.5_1.0_0.005, c(mean(err13_1.5_1.0_0.005$lm),
                                               mean(err13_1.5_1.0_0.005$lp),
                                               mean(err13_1.5_1.0_0.005$rm),
                                               mean(err13_1.5_1.0_0.005$rp),
                                               sd(err13_1.5_1.0_0.005$lm),
                                               sd(err13_1.5_1.0_0.005$lp),
                                               sd(err13_1.5_1.0_0.005$rm),
                                               sd(err13_1.5_1.0_0.005$rp),
                                               13))
err_1.5_1.0_0.005 = rbind(err_1.5_1.0_0.005, c(mean(err18_1.5_1.0_0.005$lm),
                                               mean(err18_1.5_1.0_0.005$lp),
                                               mean(err18_1.5_1.0_0.005$rm),
                                               mean(err18_1.5_1.0_0.005$rp),
                                               sd(err18_1.5_1.0_0.005$lm),
                                               sd(err18_1.5_1.0_0.005$lp),
                                               sd(err18_1.5_1.0_0.005$rm),
                                               sd(err18_1.5_1.0_0.005$rp),
                                               18))

colnames(err_1.5_1.0_0.005) = c("lm","lp","rm","rp","slm","slp","srm","srp","obs")

# growth rates
err_1.5_1.0_0.005 %>% 
  ggplot() +
  geom_point(aes(x = obs, y = lm, color = "lm")) +
  geom_errorbar(aes(x = obs, ymin=lm-slm, ymax=lm+slm), width=.2) +
  geom_point(aes(x = obs, y = lp, color = "lp")) +
  geom_errorbar(aes(x = obs, ymin=lp-slp, ymax=lp+slp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "parameter") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,2)

# switching rates
err_1.5_1.0_0.005 %>% 
  ggplot() +
  geom_point(aes(x = obs, y = rm, color = "rm")) +
  geom_errorbar(aes(x = obs, ymin=rm-srm, ymax=rm+srm), width=.2) +
  geom_point(aes(x = obs, y = rp, color = "rp")) +
  geom_errorbar(aes(x = obs, ymin=rp-srp, ymax=rp+srp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "parameter") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,0.015)

# % errors
plot1b = err_1.5_1.0_0.005 %>% 
  mutate(err_lm = abs(1.5-lm)/1.5*100,
         err_lp = abs(1.-lp)/1.*100,
         err_rm = abs(0.005-rm)/(0.005)*100,
         err_rp = abs(0.005/1.5-rp)/0.005/(1.5)*100) %>% 
  ggplot() +
  geom_jitter(aes(x = obs, y = err_lm, color = "lm"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_lp, color = "lp"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_rm, color = "rm"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_rp, color = "rp"),size=2.5,width = 0.3) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "% error") +
  theme(legend.position = "none", axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,100)

# case 2 A
err_1.2_0.01_0.001 = data.frame()
err_1.2_0.01_0.001 = rbind(err_1.2_0.01_0.001, c(mean(err3_1.2_0.01_0.001$lm),
                                               mean(err3_1.2_0.01_0.001$lp),
                                               mean(err3_1.2_0.01_0.001$rm),
                                               mean(err3_1.2_0.01_0.001$rp),
                                               sd(err3_1.2_0.01_0.001$lm),
                                               sd(err3_1.2_0.01_0.001$lp),
                                               sd(err3_1.2_0.01_0.001$rm),
                                               sd(err3_1.2_0.01_0.001$rp),
                                               3))
err_1.2_0.01_0.001 = rbind(err_1.2_0.01_0.001, c(mean(err8_1.2_0.01_0.001$lm),
                                               mean(err8_1.2_0.01_0.001$lp),
                                               mean(err8_1.2_0.01_0.001$rm),
                                               mean(err8_1.2_0.01_0.001$rp),
                                               sd(err8_1.2_0.01_0.001$lm),
                                               sd(err8_1.2_0.01_0.001$lp),
                                               sd(err8_1.2_0.01_0.001$rm),
                                               sd(err8_1.2_0.01_0.001$rp),
                                               8))
err_1.2_0.01_0.001 = rbind(err_1.2_0.01_0.001, c(mean(err13_1.2_0.01_0.001$lm),
                                               mean(err13_1.2_0.01_0.001$lp),
                                               mean(err13_1.2_0.01_0.001$rm),
                                               mean(err13_1.2_0.01_0.001$rp),
                                               sd(err13_1.2_0.01_0.001$lm),
                                               sd(err13_1.2_0.01_0.001$lp),
                                               sd(err13_1.2_0.01_0.001$rm),
                                               sd(err13_1.2_0.01_0.001$rp),
                                               13))
err_1.2_0.01_0.001 = rbind(err_1.2_0.01_0.001, c(mean(err18_1.2_0.01_0.001$lm),
                                               mean(err18_1.2_0.01_0.001$lp),
                                               mean(err18_1.2_0.01_0.001$rm),
                                               mean(err18_1.2_0.01_0.001$rp),
                                               sd(err18_1.2_0.01_0.001$lm),
                                               sd(err18_1.2_0.01_0.001$lp),
                                               sd(err18_1.2_0.01_0.001$rm),
                                               sd(err18_1.2_0.01_0.001$rp),
                                               18))

colnames(err_1.2_0.01_0.001) = c("lm","lp","rm","rp","slm","slp","srm","srp","obs")

# growth rates
err_1.2_0.01_0.001 %>% 
  ggplot() +
  geom_point(aes(x = obs, y = lm, color = "lm")) +
  geom_errorbar(aes(x = obs, ymin=lm-slm, ymax=lm+slm), width=.2) +
  geom_point(aes(x = obs, y = lp, color = "lp")) +
  geom_errorbar(aes(x = obs, ymin=lp-slp, ymax=lp+slp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "parameter") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,2)

# switching rates
err_1.2_0.01_0.001 %>% 
  ggplot() +
  geom_point(aes(x = obs, y = rm, color = "rm")) +
  geom_errorbar(aes(x = obs, ymin=rm-srm, ymax=rm+srm), width=.2) +
  geom_point(aes(x = obs, y = rp, color = "rp")) +
  geom_errorbar(aes(x = obs, ymin=rp-srp, ymax=rp+srp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "parameter") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,0.015)

# % errors
plot2a = err_1.2_0.01_0.001 %>% 
  mutate(err_lm = abs(1.2-lm)/1.2*100,
         err_lp = abs(1.2-lp)/1.2*100,
         err_rm = abs(0.01/1.2-rm)/(0.01/1.2)*100,
         err_rp = abs(0.001/1.2-rp)/(0.001/1.2)*100) %>% 
  ggplot() +
  geom_jitter(aes(x = obs, y = err_lm, color = "lm"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_lp, color = "lp"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_rm, color = "rm"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_rp, color = "rp"),size=2.5,width = 0.) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "% error") +
  theme(legend.position = "none", axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,365)

# case 2 B
err_1.2_0.005 = data.frame()
err_1.2_0.005 = rbind(err_1.2_0.005, c(mean(err3_1.2_0.005$lm),
                                                 mean(err3_1.2_0.005$lp),
                                                 mean(err3_1.2_0.005$rm),
                                                 mean(err3_1.2_0.005$rp),
                                                 sd(err3_1.2_0.005$lm),
                                                 sd(err3_1.2_0.005$lp),
                                                 sd(err3_1.2_0.005$rm),
                                                 sd(err3_1.2_0.005$rp),
                                                 3))
err_1.2_0.005 = rbind(err_1.2_0.005, c(mean(err8_1.2_0.005$lm),
                                       mean(err8_1.2_0.005$lp),
                                       mean(err8_1.2_0.005$rm),
                                       mean(err8_1.2_0.005$rp),
                                       sd(err8_1.2_0.005$lm),
                                       sd(err8_1.2_0.005$lp),
                                       sd(err8_1.2_0.005$rm),
                                       sd(err8_1.2_0.005$rp),
                                       8))
err_1.2_0.005 = rbind(err_1.2_0.005, c(mean(err13_1.2_0.005$lm),
                                       mean(err13_1.2_0.005$lp),
                                       mean(err13_1.2_0.005$rm),
                                       mean(err13_1.2_0.005$rp),
                                       sd(err13_1.2_0.005$lm),
                                       sd(err13_1.2_0.005$lp),
                                       sd(err13_1.2_0.005$rm),
                                       sd(err13_1.2_0.005$rp),
                                       13))
err_1.2_0.005 = rbind(err_1.2_0.005, c(mean(err18_1.2_0.005$lm),
                                       mean(err13_1.2_0.005$lp),
                                       mean(err13_1.2_0.005$rm),
                                       mean(err13_1.2_0.005$rp),
                                       sd(err13_1.2_0.005$lm),
                                       sd(err13_1.2_0.005$lp),
                                       sd(err13_1.2_0.005$rm),
                                       sd(err13_1.2_0.005$rp),
                                       18))

colnames(err_1.2_0.005) = c("lm","lp","rm","rp","slm","slp","srm","srp","obs")

# growth rates
err_1.2_0.005 %>% 
  ggplot() +
  geom_point(aes(x = obs, y = lm, color = "lm")) +
  geom_errorbar(aes(x = obs, ymin=lm-slm, ymax=lm+slm), width=.2) +
  geom_point(aes(x = obs, y = lp, color = "lp")) +
  geom_errorbar(aes(x = obs, ymin=lp-slp, ymax=lp+slp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "parameter") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,2)

# switching rates
err_1.2_0.005 %>% 
  ggplot() +
  geom_point(aes(x = obs, y = rm, color = "rm")) +
  geom_errorbar(aes(x = obs, ymin=rm-srm, ymax=rm+srm), width=.2) +
  geom_point(aes(x = obs, y = rp, color = "rp")) +
  geom_errorbar(aes(x = obs, ymin=rp-srp, ymax=rp+srp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "parameter") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,0.015)

# % errors
plot2b = err_1.2_0.005 %>% 
  mutate(err_lm = abs(1.2-lm)/1.2*100,
         err_lp = abs(1.2-lp)/1.2*100,
         err_rm = abs(0.005-rm)/(0.005)*100,
         err_rp = abs(0.005/1.5-rp)/0.005/(1.5)*100) %>% 
  ggplot() +
  geom_jitter(aes(x = obs, y = err_lm, color = "lm"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_lp, color = "lp"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_rm, color = "rm"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_rp, color = "rp"),size=2.5,width = 0.3) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "% error") +
  theme(legend.position = "none", axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,100)

# case 3 A
err_1.0_1.5_0.01_0.001 = data.frame()
err_1.0_1.5_0.01_0.001 = rbind(err_1.0_1.5_0.01_0.001, c(mean(err3_1.0_1.5_0.01_0.001$lm),
                                                         mean(err3_1.0_1.5_0.01_0.001$lp),
                                                         mean(err3_1.0_1.5_0.01_0.001$rm),
                                                         mean(err3_1.0_1.5_0.01_0.001$rp),
                                                         sd(err3_1.0_1.5_0.01_0.001$lm),
                                                         sd(err3_1.0_1.5_0.01_0.001$lp),
                                                         sd(err3_1.0_1.5_0.01_0.001$rm),
                                                         sd(err3_1.0_1.5_0.01_0.001$rp),
                                                         3))
err_1.0_1.5_0.01_0.001 = rbind(err_1.0_1.5_0.01_0.001, c(mean(err8_1.0_1.5_0.01_0.001$lm),
                                                         mean(err8_1.0_1.5_0.01_0.001$lp),
                                                         mean(err8_1.0_1.5_0.01_0.001$rm),
                                                         mean(err8_1.0_1.5_0.01_0.001$rp),
                                                         sd(err8_1.0_1.5_0.01_0.001$lm),
                                                         sd(err8_1.0_1.5_0.01_0.001$lp),
                                                         sd(err8_1.0_1.5_0.01_0.001$rm),
                                                         sd(err8_1.0_1.5_0.01_0.001$rp),
                                                         8))
err_1.0_1.5_0.01_0.001 = rbind(err_1.0_1.5_0.01_0.001, c(mean(err13_1.0_1.5_0.01_0.001$lm),
                                                         mean(err13_1.0_1.5_0.01_0.001$lp),
                                                         mean(err13_1.0_1.5_0.01_0.001$rm),
                                                         mean(err13_1.0_1.5_0.01_0.001$rp),
                                                         sd(err13_1.0_1.5_0.01_0.001$lm),
                                                         sd(err13_1.0_1.5_0.01_0.001$lp),
                                                         sd(err13_1.0_1.5_0.01_0.001$rm),
                                                         sd(err13_1.0_1.5_0.01_0.001$rp),
                                                         13))
err_1.0_1.5_0.01_0.001 = rbind(err_1.0_1.5_0.01_0.001, c(mean(err18_1.0_1.5_0.01_0.001$lm),
                                                         mean(err18_1.0_1.5_0.01_0.001$lp),
                                                         mean(err18_1.0_1.5_0.01_0.001$rm),
                                                         mean(err18_1.0_1.5_0.01_0.001$rp),
                                                         sd(err18_1.0_1.5_0.01_0.001$lm),
                                                         sd(err18_1.0_1.5_0.01_0.001$lp),
                                                         sd(err18_1.0_1.5_0.01_0.001$rm),
                                                         sd(err18_1.0_1.5_0.01_0.001$rp),
                                                         18))

colnames(err_1.0_1.5_0.01_0.001) = c("lm","lp","rm","rp","slm","slp","srm","srp","obs")

# growth rates
err_1.0_1.5_0.01_0.001 %>% 
  ggplot() +
  geom_point(aes(x = obs, y = lm, color = "lm")) +
  geom_errorbar(aes(x = obs, ymin=lm-slm, ymax=lm+slm), width=.2) +
  geom_point(aes(x = obs, y = lp, color = "lp")) +
  geom_errorbar(aes(x = obs, ymin=lp-slp, ymax=lp+slp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "parameter") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,2)

# switching rates
err_1.0_1.5_0.01_0.001 %>% 
  ggplot() +
  geom_point(aes(x = obs, y = rm, color = "rm")) +
  geom_errorbar(aes(x = obs, ymin=rm-srm, ymax=rm+srm), width=.2) +
  geom_point(aes(x = obs, y = rp, color = "rp")) +
  geom_errorbar(aes(x = obs, ymin=rp-srp, ymax=rp+srp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "parameter") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,0.015)

# % errors
plot3a = err_1.0_1.5_0.01_0.001 %>% 
  mutate(err_lm = abs(1.-lm)/1.*100,
         err_lp = abs(1.5-lp)/1.5*100,
         err_rm = abs(0.01/1.5-rm)/(0.01/1.5)*100,
         err_rp = abs(0.001/1.-rp)/(0.001/1.)*100) %>% 
  ggplot() +
  geom_jitter(aes(x = obs, y = err_lm, color = "lm"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_lp, color = "lp"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_rm, color = "rm"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_rp, color = "rp"),size=2.5,width = 0.3) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "% error") +
  theme(legend.position = "none", axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,100)

# case 3 B
err_1.0_1.5_0.005 = data.frame()
err_1.0_1.5_0.005 = rbind(err_1.0_1.5_0.005, c(mean(err3_1.0_1.5_0.005$lm),
                                               mean(err3_1.0_1.5_0.005$lp),
                                               mean(err3_1.0_1.5_0.005$rm),
                                               mean(err3_1.0_1.5_0.005$rp),
                                               sd(err3_1.0_1.5_0.005$lm),
                                               sd(err3_1.0_1.5_0.005$lp),
                                               sd(err3_1.0_1.5_0.005$rm),
                                               sd(err3_1.0_1.5_0.005$rp),
                                               3))
err_1.0_1.5_0.005 = rbind(err_1.0_1.5_0.005, c(mean(err8_1.0_1.5_0.005$lm),
                                       mean(err8_1.0_1.5_0.005$lp),
                                       mean(err8_1.0_1.5_0.005$rm),
                                       mean(err8_1.0_1.5_0.005$rp),
                                       sd(err8_1.0_1.5_0.005$lm),
                                       sd(err8_1.0_1.5_0.005$lp),
                                       sd(err8_1.0_1.5_0.005$rm),
                                       sd(err8_1.0_1.5_0.005$rp),
                                       8))
err_1.0_1.5_0.005 = rbind(err_1.0_1.5_0.005, c(mean(err13_1.0_1.5_0.005$lm),
                                       mean(err13_1.0_1.5_0.005$lp),
                                       mean(err13_1.0_1.5_0.005$rm),
                                       mean(err13_1.0_1.5_0.005$rp),
                                       sd(err13_1.0_1.5_0.005$lm),
                                       sd(err13_1.0_1.5_0.005$lp),
                                       sd(err13_1.0_1.5_0.005$rm),
                                       sd(err13_1.0_1.5_0.005$rp),
                                       13))
err_1.0_1.5_0.005 = rbind(err_1.0_1.5_0.005, c(mean(err18_1.0_1.5_0.005$lm),
                                       mean(err18_1.0_1.5_0.005$lp),
                                       mean(err18_1.0_1.5_0.005$rm),
                                       mean(err18_1.0_1.5_0.005$rp),
                                       sd(err18_1.0_1.5_0.005$lm),
                                       sd(err18_1.0_1.5_0.005$lp),
                                       sd(err18_1.0_1.5_0.005$rm),
                                       sd(err18_1.0_1.5_0.005$rp),
                                       18))

colnames(err_1.0_1.5_0.005) = c("lm","lp","rm","rp","slm","slp","srm","srp","obs")

# growth rates
err_1.0_1.5_0.005 %>% 
  ggplot() +
  geom_point(aes(x = obs, y = lm, color = "lm")) +
  geom_errorbar(aes(x = obs, ymin=lm-slm, ymax=lm+slm), width=.2) +
  geom_point(aes(x = obs, y = lp, color = "lp")) +
  geom_errorbar(aes(x = obs, ymin=lp-slp, ymax=lp+slp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "parameter") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,2)

# switching rates
err_1.0_1.5_0.005 %>% 
  ggplot() +
  geom_point(aes(x = obs, y = rm, color = "rm")) +
  geom_errorbar(aes(x = obs, ymin=rm-srm, ymax=rm+srm), width=.2) +
  geom_point(aes(x = obs, y = rp, color = "rp")) +
  geom_errorbar(aes(x = obs, ymin=rp-srp, ymax=rp+srp), width=.2) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "parameter") +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,0.015)

# % errors
plot3b = err_1.0_1.5_0.005 %>% 
  mutate(err_lm = abs(1.-lm)/1.*100,
         err_lp = abs(1.5-lp)/1.5*100,
         err_rm = abs(0.005/1.5-rm)/(0.005/1.5)*100,
         err_rp = abs(0.005/1.-rp)/0.005/(1.)*100) %>% 
  ggplot() +
  geom_jitter(aes(x = obs, y = err_lm, color = "lm"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_lp, color = "lp"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_rm, color = "rm"),size=2.5,width = 0.3) +
  geom_jitter(aes(x = obs, y = err_rp, color = "rp"),size=2.5,width = 0.3) +
  scale_x_continuous(breaks = c(3,8,13,18)) + 
  labs(color = NULL, x = "# obs", y = "% error") +
  theme(legend.position = "none", axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  ylim(0,100)

(plot1a + plot2a + plot3a) / (plot1b + plot2b + plot3b)

ggsave("./final_plot.png", width = 10, height = 6, dpi = 600)



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
