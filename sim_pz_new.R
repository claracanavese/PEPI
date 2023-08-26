library(LaplacesDemon)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(reshape2)
library(patchwork)
library(plotly)
library(stabledist)

Pz_exp <- function(dat,lm,lp,om,op) {
  time <- mean(dat$time)
  if (lm > lp) {
    exprate_minus <- exp(-(lm+om*op/(lm-lp))*time)
    exprate_plus <- (lm - lp)/op*exp(-(lm+om*op/(lm-lp))*time)
  } else if (lm == lp) {
    exprate_minus <- 1/(cosh(sqrt(om*op)*time))*exp(-lm*time)
    exprate_plus <- 1/(sinh(sqrt(om*op)*time))*sqrt(om/op)*exp(-lm*time)
  }
  rates <- list(exprate_minus,exprate_plus)
  return(rates)
}
my_palette <- c(rgb(102,204,102,maxColorValue = 255),"#D5D139")

# upload simulated distributions

final1a <- read.csv("./pz_sims/1.5_1.0_0.01_0.001_8t_81p.csv")
final1a <- final1a[,2:4]
final1b <- read.csv("./pz_sims/1.5_1.0_0.005_8t_81p.csv")
final1b <- final1b[,2:4]
final1c <- read.csv("./pz_sims/1.5_1.0_0.001_0.01_8t_81p.csv")
final1c <- final1c[,2:4]

final2a <- read.csv("./pz_sims/1.2_0.01_0.001_8t_81p.csv")
final2a <- final2a[,2:4]
final2b <- read.csv("./pz_sims/1.2_0.005_8t_81p.csv")
final2b <- final2b[,2:4]
final2c <- read.csv("./pz_sims/1.2_0.001_0.01_8t_81p.csv")
final2c <- final2c[,2:4]

final3a <- read.csv("./pz_sims/1.0_1.5_0.01_0.001_8t_81p.csv")
final3a <- final3a[,2:4]
final3b <- read.csv("./pz_sims/1.0_1.5_0.005_8t_81p.csv")
final3b <- final3b[,2:4]
final3c <- read.csv("./pz_sims/1.0_1.5_0.001_0.01_8t_81p.csv")
final3c <- final3c[,2:4]

# CASE 1

rate1a <- Pz_exp(final1a,1.5,1.0,0.01,0.001)
rate1b <- Pz_exp(final1b,1.5,1.0,0.005,0.005)
rate1c <- Pz_exp(final1c,1.5,1.0,0.001,0.01)

# minus
plot1ma <- final1a %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = rgb(102,204,102,maxColorValue = 255), bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1a[1])), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  labs(x="z-") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE))

plot1mb <- final1b %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = rgb(102,204,102,maxColorValue = 255), bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1b[1])), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  labs(x="z-") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE))

plot1mc <- final1c %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = rgb(102,204,102,maxColorValue = 255), bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1c[1])), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  labs(x="z-") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE))

# plus
plot1pa <- final1a %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = "#D5D139", bins = 120, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1a[2])), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
  labs(x="z+")
  #ylim(0,0.015) + xlim(0,1000)
  #scale_y_continuous(labels = function(x) format(x, scientific = TRUE),limits = c(0,1e-3)) +
  #scale_x_continuous(breaks = c(0,20000,40000),labels = function(x) format(x, scientific = TRUE),limits = c(0,4.5e4))

plot1pb <- final1b %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = "#D5D139", bins = 120, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1b[2])), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
  labs(x="z+")

plot1pc <- final1c %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = "#D5D139", bins = 120, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1c[2])), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
  labs(x="z+")

# CASE 2

rate2a <- Pz_exp(final2a,1.2,1.2,0.01,0.001)
rate2b <- Pz_exp(final2b,1.2,1.2,0.005,0.005)
rate2c <- Pz_exp(final2c,1.2,1.2,0.001,0.01)

# minus
plot2ma <- final2a %>% 
  #filter(`Z-` < 1e6) %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = rgb(102,204,102,maxColorValue = 255), bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2a[1])), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  labs(x="z-") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE))

plot2mb <- final2b %>% 
  #filter(`Z-` < 1e6) %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = rgb(102,204,102,maxColorValue = 255), bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2b[1])), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  labs(x="z-") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE))

plot2mc <- final2c %>% 
  #filter(`Z-` < 1e6) %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = rgb(102,204,102,maxColorValue = 255), bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2c[1])), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  labs(x="z-") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE))

# plus
plot2pa <- final2a %>% 
  #filter(`Z+` < 1000) %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = "#D5D139", bins = 100, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2a[2])), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  labs(x="z+") + 
  ylim(0,0.015) + xlim(0,1500)
  #scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,1e-3)) +
  #scale_x_continuous(breaks = c(0,20000,40000), labels = function(x) format(x, scientific = TRUE),limits = c(0,4.5e4))
plot2pb <- final2b %>% 
  #filter(`Z+` < 1000) %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = "#D5D139", bins = 100, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2b[2])), size=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  labs(x="z+")

plot2pc <- final2c %>% 
  #filter(`Z+` < 1000) %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = "#D5D139", bins = 100, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2c[2])), size=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  labs(x="z+")

# CASE 3

analytic1 <- read.csv("1_1.5_0.01_0.001_8t_minus.csv") %>%
  tibble::as_tibble() %>% 
  mutate(type="z_minus") %>% 
  add_row(read.csv("1_1.5_0.01_0.001_8t_plus.csv") %>% 
            tibble::as_tibble() %>% 
            mutate(type="z_plus"))

analytic2 <- read.csv("1_1.5_0.005_8t_minus.csv") %>%
  tibble::as_tibble() %>% 
  mutate(type="z_minus") %>% 
  add_row(read.csv("1_1.5_0.005_8t_plus.csv") %>% 
            tibble::as_tibble() %>% 
            mutate(type="z_plus"))

analytic3 <- read.csv("1_1.5_0.001_0.01_8t_minus.csv") %>%
  tibble::as_tibble() %>% 
  mutate(type="z_minus") %>% 
  add_row(read.csv("1_1.5_0.001_0.01_8t_plus.csv") %>% 
            tibble::as_tibble() %>% 
            mutate(type="z_plus"))

# minus
plot3ma <- final3a %>% 
  ggplot(aes(x=z_minus)) +
  geom_histogram(aes(y=after_stat(density)), bins=80, fill=rgb(102,204,102,maxColorValue = 255), na.rm = TRUE) +
  geom_line(data=analytic1 %>% filter(type=="z_minus"), aes(x=z,y=P), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  xlim(0,25000) + labs(x="z-")
plot3ma

plot3mb <- final3b %>% 
  ggplot(aes(x=z_minus)) +
  geom_histogram(aes(y=after_stat(density)), bins=80, fill=rgb(102,204,102,maxColorValue = 255), na.rm = TRUE) +
  geom_line(data=analytic2 %>% filter(type=="z_minus"), aes(x=z,y=P), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  xlim(0,25000) + labs(x="z-")

plot3mc <- final3c %>% 
  ggplot(aes(x=z_minus)) +
  geom_histogram(aes(y=after_stat(density)), bins=80, fill=rgb(102,204,102,maxColorValue = 255), na.rm = TRUE) +
  geom_line(data=analytic3 %>% filter(type=="z_minus"), aes(x=z,y=P), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  xlim(0,25000) + labs(x="z-")

# plus

plot3pa <- final3a %>% 
  ggplot(aes(x=z_plus)) +
  geom_histogram(aes(y=after_stat(density)), bins=150, fill="#D5D139", na.rm = TRUE) +
  geom_line(data=analytic1 %>% filter(type=="z_plus"), aes(x=z,y=P), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  labs(x="z+") + xlim(0,25000) + ylim(0,2e-4)
plot3p

plot3pb <- final3b %>% 
  ggplot(aes(x=z_plus)) +
  geom_histogram(aes(y=after_stat(density)), bins=150, fill="#D5D139", na.rm = TRUE) +
  geom_line(data=analytic2 %>% filter(type=="z_plus"), aes(x=z,y=P), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  labs(x="z+") + xlim(0,25000) + ylim(0,2e-4)

plot3pc <- final3c %>% 
  ggplot(aes(x=z_plus)) +
  geom_histogram(aes(y=after_stat(density)), bins=150, fill="#D5D139", na.rm = TRUE) +
  geom_line(data=analytic3 %>% filter(type=="z_plus"), aes(x=z,y=P), linewidth=0.6) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15)) +
  labs(x="z+") + xlim(0,25000) + ylim(0,2e-4)


