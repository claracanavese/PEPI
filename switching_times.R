library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(patchwork)

fs <- readRDS("./R/1.0_1.5_0.005_10t_fs.RData")
ss <- readRDS("./R/1.0_1.5_0.005_10t_ss.RData")
fs_df = data.frame(unlist(fs))
colnames(fs_df) = c("fs")
ss_df = data.frame(unlist(ss))
colnames(ss_df) = c("ss")

fs_theory <- read.csv("./R_times/1.0_1.5_0.005_fs.csv") %>%
  tibble::as_tibble()
ss_theory <- read.csv("./R_times/1.0_1.5_0.005_ss.csv") %>%
  tibble::as_tibble()

ggplot() +
  #geom_histogram(data = fs_df, aes(x = fs, y = after_stat(density)), bins = 70) 
  #geom_line(data = fs_theory, aes(x = fs, y = p), linewidth = 1) +
  geom_histogram(data = ss_df, aes(x = ss, y = after_stat(density)), bins = 60) +
  geom_line(data = ss_theory, aes(x = ss, y = p), linewidth = 1) 

median(unlist(fs), na.rm = TRUE)
sd(unlist(fs), na.rm = TRUE)
median(unlist(ss), na.rm = TRUE)
sd(unlist(ss), na.rm = TRUE)

df1_theory <- data.frame(case = c(1,1,2,2,3,3),times = c(4.875,7.484,3.803,6.873,3.340,7.484))
df1_sim <- data.frame(case = c(1,1,2,2,3,3), times = c(4.92,7.58,3.81,6.95,3.31,7.42), errors = c(1.15,1.03,1.20,1.11,1.15,1.19))

plot1 <- ggplot() +
  geom_jitter(data=df1_theory, aes(x=case,y=times,color="theory"),size=4, shape = 17, height = 0, width = 0.05) + 
  geom_point(data=df1_sim, aes(x=case,y=times,color="simulation"),size=3) +
  geom_errorbar(data=df1_sim, aes(x=case, ymin=times-errors, ymax=times+errors),width=.08,color="black") +
  scale_x_continuous(breaks = c(1,2,3)) +
  scale_color_manual(values=c("black","#BE202F")) +
  labs(col=NULL) +
  guides(colour = guide_legend(override.aes = list(shape = c(16,17)))) +
  labs(y="t") + ylim(2,10) +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.position = "none")
plot1

fs <- readRDS("./R/1.2_0.001_0.01_10t_fs.RData")
ss <- readRDS("./R/1.2_0.001_0.01_10t_ss.RData")
median(unlist(fs), na.rm = TRUE)
sd(unlist(fs), na.rm = TRUE)
median(unlist(ss), na.rm = TRUE)
sd(unlist(ss), na.rm = TRUE)

df2_theory <- data.frame(case = c(1,1,2,2,3,3),times = c(5.908,8.012,5.567,7.323,3.990,8.012))
df2_sim <- data.frame(case = c(1,1,2,2,3,3), times = c(5.99,8.48,4.54,7.76,3.97,8.14), errors = c(1.52,1.18,1.40,1.27,1.44,1.14))

plot2 <- ggplot() +
  geom_jitter(data=df2_theory, aes(x=case,y=times, color = "Theory"),size=4, shape = 17,height = 0, width = 0.03) + 
  geom_point(data=df2_sim, aes(x=case,y=times,color="Simulation"),size=3) +
  geom_errorbar(data=df2_sim, aes(x=case, ymin=times-errors, ymax=times+errors),width=.08) +
  scale_x_continuous(breaks = c(1,2,3)) +
  scale_color_manual(values=c("black","#BE202F")) +
  labs(y="t", col = NULL) + ylim(2,10) +
  theme(legend.text = element_text(size = 16), axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.position = "none")
plot2

fs <- readRDS("./R/1.0_1.5_0.001_0.01_10t_fs.RData")
ss <- readRDS("./R/1.0_1.5_0.001_0.01_10t_ss.RData")
median(unlist(fs), na.rm = TRUE)
sd(unlist(fs), na.rm = TRUE)
median(unlist(ss), na.rm = TRUE)
sd(unlist(ss), na.rm = TRUE)

df3_theory <- data.frame(case = c(1,1,2,2,3,3),times = c(6.909,9.366,5.303,8.223,4.615,8.607))
df3_sim <- data.frame(case = c(1,1,2,2,3,3), times = c(6.74,8.65,5.37,8.10,4.48,8.24), errors = c(1.56,1.15,1.75,1.33,1.61,1.27))

plot3 <- ggplot() +
  geom_jitter(data=df3_theory, aes(x=case,y=times,color="Theory"),size=4, shape = 17,height = 0, width = 0.05) + 
  geom_point(data=df3_sim, aes(x=case,y=times,color="Simulation"),size=3) +
  geom_errorbar(data=df3_sim, aes(x=case, ymin=times-errors, ymax=times+errors),width=.08) +
  scale_x_continuous(breaks = c(1,2,3)) +
  scale_color_manual(values=c("black","#BE202F")) +
  labs(y="t", col = NULL) + ylim(2,10) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 18)) +
  theme(legend.position = "none")
plot3
ggsave("./switching_times_legend.png",dpi=600)

plot1 + plot2 + plot3
ggsave("./switching_times.png",width = 12, height = 4,dpi=600)

# cumulative distribution plot
omega_m = 0.01
omega_p = 0.1
s <- 1; v <- 0.5
x <- seq(-1.1, 1.1, length = 201)

x_list = lapply(seq(0, 1.8, length = 100), function(t){
  return(-omega_m*2*exp(20*t)/20)
})
t_list = seq(0, 1.8, length = 100)
x_num <- as.numeric(unlist(x_list))

plot(t_list, lerch(x_num, s = s, v = v), type = "l", col = "blue",
     las = 1, main = paste0("lerch(x, s = ", s,", v = ", v, ")"))
# abline(v = 0, h = 1, lty = "dashed", col = "gray")

C2 <- function(t) {
  lerchphi = lerch(-omega_m*2*exp(20*t)/20, s = 1, v = 0.5)
  return(omega_p*omega_m/400*2*exp(20*t)*lerchphi)
}
C2(0.1)
C2(0.5)
plot(t_list, time_cumulative3(t_list), type = "l", col = "blue",
     las = 1, main = paste0("P(T2)>t"), xlab="t", ylab="C2")

lerch(-omega_m*2*exp(20*0.5)/20, s = 1, v = 0.5, tolerance = 1.0e-10, iter = 1000)
