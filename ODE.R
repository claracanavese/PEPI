library(deSolve)
library(RColorBrewer)
library(reshape2)
library(magrittr)
library(dplyr)
library(ggplot2)
my_palette <- c("#72B8B5","#FFCB0A","#265450")

# (co)variances
covariances <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    dM1 <- lambda_min*M1 + omega_min*M2
    dM2 <- lambda_plus*M2 + omega_plus*M1
    dS1 <- 2*lambda_min*S1 + (alpha_min+beta_min)*M1 + 2*omega_min*C + omega_min*M2
    dS2 <- 2*lambda_plus*S2 + (alpha_plus+beta_plus)*M2 + 2*omega_plus*C + omega_plus*M1
    dC <- (lambda_min + lambda_plus)*C + omega_plus*S1 + omega_min*S2
    list(c(dM1, dM2, dS1, dS2, dC))
  })
}

covariances2 <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    dM1 <- lambda_min*M1 + omega_min*M2
    dM2 <- lambda_plus*M2 + omega_plus*M1
    dV1 <- 2*lambda_min*V1 + 2*omega_min*C + (alpha_min+beta_min)*M1 + omega_min*M2
    dV2 <- 2*lambda_plus*V2 + 2*omega_plus*C + (alpha_plus+beta_plus)*M2 + omega_plus*M1
    dC <- (lambda_min + lambda_plus)*C + omega_plus*V1 + omega_min*V2
    list(c(dM1, dM2, dV1, dV2, dC))
  })
}

state <- c(M1 = 1, M2 = 0, S1 = 1, S2 = 0, C = 0)
state2 <- c(M1 = 1, M2 = 0, V1 = 0, V2 = 0, C = 0)

options(scipen = 0)

parameters <- c(lambda_min = 1.5, lambda_plus = 1.0, omega_min = 0.001, omega_plus = 0.01, alpha_min = 1.5, beta_min = 0, alpha_plus = 1.0, beta_plus = 0)
out1 <- ode(y = state, times = seq(0, 20, by = 0.01), func = covariances, parms = parameters)
out2 <- ode(y = state2, times = seq(0, 20, by = 0.01), func = covariances2, parms = parameters)
out1_df <- data.frame(t = out1[,1], M1 = out1[,2], M2 = out1[,3], S1 = out1[,4], S2 = out1[,5], C = out1[,6])
out2_df <- data.frame(t = out2[,1], M1 = out2[,2], M2 = out2[,3], V1 = out2[,4], V2 = out2[,5], C = out2[,6])

out1_df = out1_df %>% mutate(V1 = S1 - M1**2,V2 = S2 - M2**2)


# case 1
parameters_cov1a <- c(lambda_min = 1.5, lambda_plus = 1.0, omega_min = 0.001, omega_plus = 0.01, alpha_min = 1.5, beta_min = 0, alpha_plus = 1.0, beta_plus = 0)
parameters_cov1b <- c(lambda_min = 1.5, lambda_plus = 1.0, omega_min = 0.005, omega_plus = 0.005, alpha_min = 1.5, beta_min = 0, alpha_plus = 1.0, beta_plus = 0)
parameters_cov1c <- c(lambda_min = 1.5, lambda_plus = 1.0, omega_min = 0.01, omega_plus = 0.001, alpha_min = 1.5, beta_min = 0, alpha_plus = 1.0, beta_plus = 0)
out1a <- ode(y = state, times = seq(0, 20, by = 0.01), func = covariances, parms = parameters_cov1a)
out1b <- ode(y = state, times = seq(0, 20, by = 0.01), func = covariances, parms = parameters_cov1b)
out1c <- ode(y = state, times = seq(0, 20, by = 0.01), func = covariances, parms = parameters_cov1c)
out1a_df <- data.frame(t = out1a[,1], M1 = out1a[,2], M2 = out1a[,3], S1 = out1a[,4], S2 = out1a[,5], C = out1a[,6])
out1b_df <- data.frame(t = out1b[,1], M1 = out1b[,2], M2 = out1b[,3], S1 = out1b[,4], S2 = out1b[,5], C = out1b[,6])
out1c_df <- data.frame(t = out1c[,1], M1 = out1c[,2], M2 = out1c[,3], S1 = out1c[,4], S2 = out1c[,5], C = out1c[,6])

# case 2
parameters_cov2a <- c(lambda_min = 1.2, lambda_plus = 1.2, omega_min = 0.001, omega_plus = 0.01, alpha_min = 1.2, beta_min = 0, alpha_plus = 1.2, beta_plus = 0)
parameters_cov2b <- c(lambda_min = 1.2, lambda_plus = 1.2, omega_min = 0.005, omega_plus = 0.005, alpha_min = 1.2, beta_min = 0, alpha_plus = 1.2, beta_plus = 0)
parameters_cov2c <- c(lambda_min = 1.2, lambda_plus = 1.2, omega_min = 0.01, omega_plus = 0.001, alpha_min = 1.2, beta_min = 0, alpha_plus = 1.2, beta_plus = 0)
out2a <- ode(y = state, times = seq(0, 50, by = 0.01), func = covariances, parms = parameters_cov2a)
out2b <- ode(y = state, times = seq(0, 50, by = 0.01), func = covariances, parms = parameters_cov2b)
out2c <- ode(y = state, times = seq(0, 50, by = 0.01), func = covariances, parms = parameters_cov2c)
out2a_df <- data.frame(t = out2a[,1], M1 = out2a[,2], M2 = out2a[,3], S1 = out2a[,4], S2 = out2a[,5], C = out2a[,6])
out2b_df <- data.frame(t = out2b[,1], M1 = out2b[,2], M2 = out2b[,3], S1 = out2b[,4], S2 = out2b[,5], C = out2b[,6])
out2c_df <- data.frame(t = out2c[,1], M1 = out2c[,2], M2 = out2c[,3], S1 = out2c[,4], S2 = out2c[,5], C = out2c[,6])

# case 3
parameters_cov3a <- c(lambda_min = 1.0, lambda_plus = 1.5, omega_min = 0.001, omega_plus = 0.01, alpha_min = 1.0, beta_min = 0, alpha_plus = 1.5, beta_plus = 0)
parameters_cov3b <- c(lambda_min = 1.0, lambda_plus = 1.5, omega_min = 0.005, omega_plus = 0.05, alpha_min = 1.0, beta_min = 0, alpha_plus = 1.5, beta_plus = 0)
parameters_cov3c <- c(lambda_min = 1.0, lambda_plus = 1.5, omega_min = 0.01, omega_plus = 0.001, alpha_min = 1.0, beta_min = 0, alpha_plus = 1.5, beta_plus = 0)
out3a <- ode(y = state, times = seq(0, 50, by = 0.01), func = covariances, parms = parameters_cov3a)
out3b <- ode(y = state, times = seq(0, 50, by = 0.01), func = covariances, parms = parameters_cov3b)
out3c <- ode(y = state, times = seq(0, 50, by = 0.01), func = covariances, parms = parameters_cov3c)
out3a_df <- data.frame(t = out3a[,1], M1 = out3a[,2], M2 = out3a[,3], S1 = out3a[,4], S2 = out3a[,5], C = out3a[,6])
out3b_df <- data.frame(t = out3b[,1], M1 = out3b[,2], M2 = out3b[,3], S1 = out3b[,4], S2 = out3b[,5], C = out3b[,6])
out3c_df <- data.frame(t = out3c[,1], M1 = out3c[,2], M2 = out3c[,3], S1 = out3c[,4], S2 = out3c[,5], C = out3c[,6])

simulation <- read.csv("./GitHub/switching_process/Gillespy2/1.5_1.2_0.015_0.005_5t_51p/simulations/switching_results_avg.csv") %>% tibble::as_tibble()

pmin = ggplot() + 
  #geom_line(data = out1_df, aes(x=t,y=M1, color = "ode"), linewidth = 0.5) +
  geom_point(data = out1_df, aes(x=t,y=M1, color = "ode"), size = 0.8) +
  geom_line(data = out2_df, aes(x=t,y=M1, color = "regression"), linewidth = 0.5, color = 'black') +
  #geom_line(data = out3_df, aes(x=t,y=M1, color = "regression")) +
  #geom_point(data = simulation, aes(x = time, y = z_minus), size = 0.8) +
  #geom_point(data = simulation_py1, aes(x = time, y = z_minus), size = 0.8, shape = 15) +
  ggtitle("Z-(t)") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  scale_color_discrete(name = "")

pplus = ggplot() + 
  #geom_line(data = out1_df, aes(x=t,y=M2, color="ode")) +
  geom_point(data = out1_df, aes(x=t,y=M2, color="ode"), size = 0.8) +
  geom_line(data = out2_df, aes(x=t,y=M2, color="regression"), color = 'black') +
  #geom_line(data = out3_df, aes(x=t,y=M2, color = "regression")) +
  #geom_point(data = simulation, aes(x = time, y = z_plus), size = 0.8) +
  #geom_point(data = simulation_py1, aes(x = time, y = z_plus), size = 0.8, shape = 15) +
  ggtitle("Z+(t)") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  scale_color_discrete(name = "")

pmin
pplus
pmin / pplus
ggsave("./GitHub/switching_process/Gillespy2/1.5_1.2_0.015_0.005_5t_51p/gamma_1.5_280/odeVSregression_ode_zoom.png", width = 8, height = 7, dpi = 600)

# plot standard deviation over mean
plotsm1a <- out1a_df %>% 
  mutate(V1 = S1 - M1**2,V2 = S2 - M2**2) %>% 
  mutate(D1 = sqrt(V1), D2 = sqrt(V2)) %>% 
  mutate(R1 = D1/M1, R2 = D2/M2) %>% 
  ggplot() +
  geom_line(aes(x = t,y = R1, color = "z-"), linewidth = 1) +
  geom_line(aes(x = t,y = R2, color = "z+"), linewidth = 1) +
  labs(x = "t", y = expression(sigma/mu), color = NULL) + ylim(0,2) +
  scale_color_manual(values=c(my_palette[1],my_palette[2])) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.position="none")

plotsm1b <- out1b_df %>% 
  mutate(V1 = S1 - M1**2,V2 = S2 - M2**2) %>%
  mutate(D1 = sqrt(V1), D2 = sqrt(V2)) %>% 
  mutate(R1 = D1/M1, R2 = D2/M2) %>% 
  ggplot() +
  geom_line(aes(x = t,y = R1, color = "z-"), linewidth = 1) +
  geom_line(aes(x = t,y = R2, color = "z+"), linewidth = 1) +
  labs(x = "t", y = expression(sigma/mu), color = NULL) + ylim(0,2) +
  scale_color_manual(values=c(my_palette[1],my_palette[2])) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.position="none")

plotsm1c <- out1c_df %>% 
  mutate(V1 = S1 - M1**2,V2 = S2 - M2**2) %>% 
  mutate(D1 = sqrt(V1), D2 = sqrt(V2)) %>% 
  mutate(R1 = D1/M1, R2 = D2/M2) %>% 
  ggplot() +
  geom_line(aes(x = t,y = R1, color = "z-"), linewidth = 1) +
  geom_line(aes(x = t,y = R2, color = "z+"), linewidth = 1) +
  labs(x = "t", y = expression(sigma/mu), color = NULL) + ylim(0,2) +
  scale_color_manual(values=c(my_palette[1],my_palette[2])) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.position="none") 
  #theme(legend.text = element_text(size = 16))
plotsm1a / plotsm1b / plotsm1c
ggsave("./variance_first_case.png",  width = 4, height = 10, dpi = 600)

plotsm2a <- out2a_df %>% 
  mutate(V1 = S1 - M1**2,V2 = S2 - M2**2) %>% 
  mutate(D1 = sqrt(V1), D2 = sqrt(V2)) %>% 
  mutate(R1 = D1/M1, R2 = D2/M2) %>% 
  ggplot() +
  geom_line(aes(x = t,y = R1, color = "z-"), linewidth = 1) +
  geom_line(aes(x = t,y = R2, color = "z+"), linewidth = 1) +
  labs(x = "t", y = expression(sigma/mu), color = NULL) + ylim(0,2) +
  scale_color_manual(values=c(my_palette[1],my_palette[2])) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.position="none") 
plotsm2b <- out2b_df %>% 
  mutate(V1 = S1 - M1**2,V2 = S2 - M2**2) %>%
  mutate(D1 = sqrt(V1), D2 = sqrt(V2)) %>% 
  mutate(R1 = D1/M1, R2 = D2/M2) %>% 
  ggplot() +
  geom_line(aes(x = t,y = R1, color = "z-"), linewidth = 1) +
  geom_line(aes(x = t,y = R2, color = "z+"), linewidth = 1) +
  labs(x = "t", y = expression(sigma/mu), color = NULL) + ylim(0,2) +
  scale_color_manual(values=c(my_palette[1],my_palette[2])) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.position="none") 
plotsm2c <- out2c_df %>% 
  mutate(V1 = S1 - M1**2,V2 = S2 - M2**2) %>% 
  mutate(D1 = sqrt(V1), D2 = sqrt(V2)) %>% 
  mutate(R1 = D1/M1, R2 = D2/M2) %>% 
  ggplot() +
  geom_line(aes(x = t,y = R1, color = "z-"), linewidth = 1) +
  geom_line(aes(x = t,y = R2, color = "z+"), linewidth = 1) +
  labs(x = "t", y = expression(sigma/mu), color = NULL) + ylim(0,2) +
  scale_color_manual(values=c(my_palette[1],my_palette[2])) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.position="none") 
plotsm2a / plotsm2b / plotsm2c
ggsave("./variance_second_case.png",  width = 4, height = 10, dpi = 600)

plotsm3a <- out3a_df %>% 
  mutate(V1 = S1 - M1**2,V2 = S2 - M2**2) %>% 
  mutate(D1 = sqrt(V1), D2 = sqrt(V2)) %>% 
  mutate(R1 = D1/M1, R2 = D2/M2) %>% 
  ggplot() +
  geom_line(aes(x = t,y = R1, color = "z-"), linewidth = 1) +
  geom_line(aes(x = t,y = R2, color = "z+"), linewidth = 1) +
  labs(x = "t", y = expression(sigma/mu), color = NULL) + ylim(0,20) +
  scale_color_manual(values=c(my_palette[1],my_palette[2])) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.position="none") 
plotsm3b <- out3b_df %>% 
  mutate(V1 = S1 - M1**2,V2 = S2 - M2**2) %>%
  mutate(D1 = sqrt(V1), D2 = sqrt(V2)) %>% 
  mutate(R1 = D1/M1, R2 = D2/M2) %>% 
  ggplot() +
  geom_line(aes(x = t,y = R1, color = "z-"), linewidth = 1) +
  geom_line(aes(x = t,y = R2, color = "z+"), linewidth = 1) +
  labs(x = "t", y = expression(sigma/mu), color = NULL) + ylim(0,20) +
  scale_color_manual(values=c(my_palette[1],my_palette[2])) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.position="none") 
plotsm3c <- out3c_df %>% 
  mutate(V1 = S1 - M1**2,V2 = S2 - M2**2) %>% 
  mutate(D1 = sqrt(V1), D2 = sqrt(V2)) %>% 
  mutate(R1 = D1/M1, R2 = D2/M2) %>% 
  ggplot() +
  geom_line(aes(x = t,y = R1, color = "z-"), linewidth = 1) +
  geom_line(aes(x = t,y = R2, color = "z+"), linewidth = 1) +
  labs(x = "t", y = expression(sigma/mu), color = NULL) + ylim(0,20) +
  scale_color_manual(values=c(my_palette[1],my_palette[2])) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18), legend.text = element_text(size=22)) +
  #guides(color = guide_legend(keywidth = unit(1, "cm")))
  theme(legend.position="none") 
plotsm3a / plotsm3b / plotsm3c
ggsave("./variance_legend.png", width = 6, height = 6)
ggsave("./variance_third_case.png",  width = 4, height = 10, dpi = 600)

(plotsm1a + plotsm1b + plotsm1c) / (plotsm2a + plotsm2b + plotsm2c) / (plotsm3a + plotsm3b + plotsm3c)



ggplot(out_df) +
  geom_line(aes(x = t, y = M1, color = "mean1"), linewidth = 1) +
  geom_ribbon(aes(x = t, ymin = M1 - D1, ymax = M1 + D1), fill = "red", alpha = 0.2)

ggplot(out_df) +
  geom_line(aes(x = t, y = M2, color = "mean2"), linewidth = 1, color = "blue") +
  geom_ribbon(aes(x = t, ymin = M2 - D2, ymax = M2 + D2), fill = "blue", alpha = 0.2)
  
zmin_ode <- function(t, z0, lambda_minus, lambda_plus, omega_minus, omega_plus){
  delta = (lambda_minus - lambda_plus)^2 + 4*omega_minus*omega_plus
  c1 = ((lambda_minus - lambda_plus + sqrt(delta))*z0[1] + 2*omega_minus*z0[2])/((lambda_minus - lambda_plus + sqrt(delta))^2 + 4*omega_minus*omega_plus)
  c2 = (2*omega_plus*z0[1] - (lambda_minus - lambda_plus + sqrt(delta))*z0[2])/((lambda_minus - lambda_plus + sqrt(delta))^2 + 4*omega_minus*omega_plus)
  zmin = exp((lambda_minus + lambda_plus)*t/2.)*(c1*(lambda_minus - lambda_plus + sqrt(delta))*exp(sqrt(delta)*t/2.) + c2*2*omega_minus*exp(-sqrt(delta)*t/2.))
  return(zmin)
}

zplus_ode <- function(t, z0, lambda_minus, lambda_plus, omega_minus, omega_plus){
  delta = (lambda_minus - lambda_plus)^2 + 4*omega_minus*omega_plus
  c1 = ((lambda_minus - lambda_plus + sqrt(delta))*z0[1] + 2*omega_minus*z0[2])/((lambda_minus - lambda_plus + sqrt(delta))^2 + 4*omega_minus*omega_plus)
  c2 = (2*omega_plus*z0[1] - (lambda_minus - lambda_plus + sqrt(delta))*z0[2])/((lambda_minus - lambda_plus + sqrt(delta))^2 + 4*omega_minus*omega_plus)
  zplus = exp((lambda_minus + lambda_plus)*t/2.)*(c1*2*omega_plus*exp(sqrt(delta)*t/2.) + c2*(lambda_plus - lambda_minus - sqrt(delta))*exp(-sqrt(delta)*t/2.))
  return(zplus)
}

z0 <- as.array(c(1000,100))
t <- seq(0,10, length=100)
input <- lapply(t, function(time) {
  dplyr::tibble(
    zmin = zmin_ode(time, z0 = z0, lambda_minus = 1.5, lambda_plus = 1.0, omega_minus = .001, omega_plus = .01),
    zplus = zplus_ode(time, z0, lambda_minus = 1.5, lambda_plus = 1.0, omega_minus = .001, omega_plus = .01)
    )
}) %>% do.call('bind_rows', .)
output <- lapply(t, function(time) {
  dplyr::tibble(
    zmin = zmin_ode(time, z0 = z0, lambda_minus = 1.504, lambda_plus = 1.06, omega_minus = 0.005, omega_plus = 0.0084),
    zplus = zplus_ode(time, z0, lambda_minus = 1.504, lambda_plus = 1.06, omega_minus = 0.005, omega_plus = 0.0084)
  )
}) %>% do.call('bind_rows', .)

sim <- read.csv("./PEPI/sim_1.5_1.0_0.001_0.01/simulation_5.csv")

#sim <- sim[1:(nrow(sim)-1),]
input$t <- t
output$t <- t
input %>% 
  tidyr::pivot_longer(!t) %>% 
  ggplot(mapping = aes(x=t, y=value, col=name)) +
  geom_point()

compmin = ggplot() +
  geom_line(data = input, aes(x = t, y = zmin, color = "ode_min"), size = 0.8) +
  geom_line(data = output, aes(x = t, y = zmin, color = "inf_min"), size = 0.8) +
  geom_point(data = sim, aes(x = time, y = z_minus, color = "sim"), size = 0.8)
  # geom_point(data = sim1, aes(x = time, y = z_minus, color = "sim_1"), size = 0.8) +
  # geom_point(data = sim2, aes(x = time, y = z_minus, color = "sim_2"), size = 0.8) +
  # geom_point(data = sim3, aes(x = time, y = z_minus, color = "sim_3"), size = 0.8) +
  # geom_point(data = sim4, aes(x = time, y = z_minus, color = "sim_4"), size = 0.8)
  # geom_point(data = sim6, aes(x = time, y = zm, color = "sim_6"), size = 0.5) +
  # geom_point(data = sim7, aes(x = time, y = zm, color = "sim_7"), size = 0.5) +
  # geom_point(data = sim8, aes(x = time, y = zm, color = "sim_8"), size = 0.5) +
  # geom_point(data = sim9, aes(x = time, y = zm, color = "sim_9"), size = 0.5)
  
compplus = ggplot() +
  geom_line(data = input, aes(x = t, y = zplus, color = "ode_plus"), linewidth = 0.8) +
  geom_line(data = output, aes(x = t, y = zplus, color = "inf_plus"), size = 0.8) +
  geom_point(data = sim, aes(x = time, y = z_plus, color = "sim"), size = 0.8)
  # geom_point(data = sim1, aes(x = time, y = z_plus, color = "sim_1"), size = 0.8) +
  # geom_point(data = sim2, aes(x = time, y = z_plus, color = "sim_2"), size = 0.8) +
  # geom_point(data = sim3, aes(x = time, y = z_plus, color = "sim_3"), size = 0.8) +
  # geom_point(data = sim4, aes(x = time, y = z_plus, color = "sim_4"), size = 0.8)
  # geom_point(data = sim6, aes(x = time, y = zp, color = "sim_6"), size = 0.5) +
  # geom_point(data = sim7, aes(x = time, y = zp, color = "sim_7"), size = 0.5) +
  # geom_point(data = sim8, aes(x = time, y = zp, color = "sim_8"), size = 0.5) +
  # geom_point(data = sim9, aes(x = time, y = zp, color = "sim_9"), size = 0.5)

compmin + compplus
  
zmin_ode(0, z0, 1.5, 1.0, .01, .01)

y = lapply(t, zplus_ode, z0=z0, lambda_minus=1.5, lambda_plus=1.0, omega_minus=0.01, omega_plus=0.01) %>% unlist()

plot(t, y)

py_simulation <- read.csv("./GitHub/switching_process/Gillespy2/1.5_1.0_005_001/switching_results_avg.csv") %>%
  tibble::as_tibble()
colnames(py_simulation) <- c("step","t","Z-","Z+")


ggplot() +
  geom_point(data = out_df, aes(x = t, y = M1), size = 0.5) +
  stat_function(fun = zmin_ode, args = list(z0, lambda_minus = 1.5, lambda_plus = 1.0, omega_minus = 0.01, omega_plus = 0.01)) +
  stat_function(fun = zmin_ode, args = list(z0, lambda_minus = 1.5, lambda_plus = 0.938, omega_minus = 0.02, omega_plus = 0.013), color = "red")

ggplot() +
  geom_point(data = out_df, aes(x = t, y = M2), size = 0.5) +
  stat_function(fun = zplus_ode, args = list(z0, lambda_minus = 1.5, lambda_plus = 1.0, omega_minus = 0.01, omega_plus = 0.01)) +
  stat_function(fun = zplus_ode, args = list(z0, lambda_minus = 1.5, lambda_plus = 0.938, omega_minus = 0.02, omega_plus = 0.013), color = "red")


omega_plus = c(0.0, 0.01, 0.05, 0.5)

ggplot() +
  stat_function(fun = zmin_ode, args = list(z0, lambda_minus = 1.5, lambda_plus = 1.0, omega_minus = 0.01, omega_plus = omega_plus[1]), color = 'red') +
  stat_function(fun = zmin_ode, args = list(z0, lambda_minus = 1.5, lambda_plus = 1.0, omega_minus = 0.01, omega_plus = omega_plus[2]), color = "blue") +
  stat_function(fun = zmin_ode, args = list(z0, lambda_minus = 1.5, lambda_plus = 1.0, omega_minus = 0.01, omega_plus = omega_plus[3]), color = "green") +
  #stat_function(fun = zmin_ode, args = list(z0, lambda_minus = 1.5, lambda_plus = 1.0, omega_minus = 0.01, omega_plus = omega_plus[4])) +
  xlim(0,4)

ggplot() +
  stat_function(fun = zmin_ode, args = list(z0, lambda_minus = 1.5, lambda_plus = 1.0, omega_minus = 0.01, omega_plus = omega_plus[1]), color = 'red') +
  stat_function(fun = zplus_ode, args = list(z0, lambda_minus = 1.5, lambda_plus = 1.0, omega_minus = 0.01, omega_plus = omega_plus[1]), color = 'red')
  
ggplot() +
  stat_function(fun = zmin_ode, args = list(z0, lambda_minus = 1.5, lambda_plus = 1.0, omega_minus = 0.01, omega_plus = omega_plus[2]), color = 'blue') +
  stat_function(fun = zplus_ode, args = list(z0, lambda_minus = 1.5, lambda_plus = 1.0, omega_minus = 0.01, omega_plus = omega_plus[2]), color = 'blue')

ggplot() +
  stat_function(fun = zmin_ode, args = list(z0, lambda_minus = 1.0, lambda_plus = 1.5, omega_minus = 0.001, omega_plus = 0.01), color = 'blue') +
  stat_function(fun = zplus_ode, args = list(z0, lambda_minus = 1.0, lambda_plus = 1.5, omega_minus = 0.001, omega_plus = 0.01), color = 'blue') +
  geom_point(data = py_simulation, aes(x = t, y = `Z-`)) +
  #geom_point(data = py_simulation, aes(x = t, y = `Z+`)) +
  #stat_function(fun = zmin_ode, args = list(z0, lambda_minus = 0.995, lambda_plus = 1.474, omega_minus = 0.016, omega_plus = 0.015), color = 'red') +
  stat_function(fun = zplus_ode, args = list(z0, lambda_minus = 0.995, lambda_plus = 1.474, omega_minus = 0.016, omega_plus = 0.015), color = 'red') +
  xlim(1,4)

# rho, sigma
switching_process1 <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    drho <- rho*rho*(lambda_plus + omega_min - lambda_min - omega_plus) + rho*(lambda_min - lambda_plus - 2*omega_min) + omega_min
    dsigma <- sigma*((lambda_min + omega_plus - lambda_plus - omega_min)*rho + lambda_plus + omega_min)
    list(c(drho, dsigma))
  })
}

parameters <- c(lambda_min = 15, lambda_plus = 10, omega_min = 0.01, omega_plus = 0.1)

state <- c(rho = 1, sigma = 1)
times <- seq(0, 2, by = 0.001)
out <- ode(y = state, times = times, func = switching_process1, parms = parameters)
plot(out)

switching_process1 <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    drho <- (lambda_min - omega_plus)*rho + omega_min*(1 - rho) - rho*(lambda_min*rho + lambda_plus*(1 - rho))
    dsigma <- sigma*(lambda_min*rho + lambda_plus*(1 - rho))
    list(c(drho, dsigma))
  })
}

# x,y
switching_process2 <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- lambda_min*X + omega_min*Y
    dY <- lambda_plus*Y + omega_plus*X
    list(c(dX, dY))
  })
}
state <- c(X = 1000, Y = 100)
times <- seq(0, 9., by = 0.01)
parameters1 <- c(lambda_min = 1.2, lambda_plus = 1.2, omega_min = 0.001, omega_plus = 0.01)
parameters2 <- c(lambda_min = 1.5, lambda_plus = 0.768, omega_min = 0.02, omega_plus = 0.072)
out2 <- ode(y = state, times = t, func = switching_process2, parms = parameters1)
out2b <- ode(y = state, times = times, func = switching_process2, parms = parameters2)
plot(out2)

out2_df <- data.frame(time = out2[,1], z_minus = out2[,2], z_plus = out2[,3])
out2b_df <- data.frame(t = out2b[,1], ZM = out2b[,2], ZP = out2b[,3])
ggplot() + 
  geom_line(data = out2_df, aes(x=t,y=z_minus),color="blue") + 
  geom_line(data = out2_df, aes(x=t,y=z_plus),color="red") +
  geom_point(data = input, aes(x=t,y=zmin),color="blue") + 
  geom_point(data = input, aes(x=t,y=zplus),color="red") +
  ylab("Z")

n_obs <- rpois(n = length(times),
               lambda = out[,2])
plot(n_obs ~ times, xlab = "Time", ylab = "Z-")
points(times, out[,2], type = "l", lwd=2)

saveRDS(out2_df, file = paste0("./ode_1.5_1.0_0.01_0.001_10t.rds"))

