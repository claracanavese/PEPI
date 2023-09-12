
lm = 0; lp = 0; om = 0; op = 0;
for (i in seq(6,900, by = 9)) {
  fit = readRDS(paste0("./fit_8/fit_",i,".rds"))
  lm = lm + mean(unlist(rstan::extract(fit, pars = c("lambda_minus"))))
  lp = lp + mean(unlist(rstan::extract(fit, pars = c("lambda_plus"))))
  om = om + mean(unlist(rstan::extract(fit, pars = c("omega_minus"))))
  op = op + mean(unlist(rstan::extract(fit, pars = c("omega_plus"))))
  print(i)
}

err_lm = abs(1.0 - lm/100)/1.0
err_lp = abs(1.5 - lp/100)/1.5
err_om = abs(0.01 - om/100)/0.01
err_op = abs(0.001 - op/100)/0.001

err8_5 = c(err_lm, err_lp, err_om, err_op)
err8_4 = c(err_lm, err_lp, err_om, err_op)
err8_3 = c(err_lm, err_lp, err_om, err_op)
err8_2 = c(err_lm, err_lp, err_om, err_op)
err8_1 = c(err_lm, err_lp, err_om, err_op)



