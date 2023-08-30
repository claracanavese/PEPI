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
    exprate_minus <- exp(-lm*time)/(cosh(sqrt(om*op)*time))
    exprate_plus <- exp(-lm*time)*sqrt(om/op)/(sinh(sqrt(om*op)*time))
  }
  rates <- list(exprate_minus,exprate_plus)
  return(rates)
}
my_palette <- c("#72B8B5","#FFCB0A","#265450")

# upload simulated distributions

final1a <- read.csv("./pz_sims/1.5_1.0_0.01_0.001_8t_81p.csv")
final1a <- final1a[,2:4]
final1b <- read.csv("./pz_sims/1.5_1.0_0.005_8t_81p.csv")
final1b <- final1b[,2:4]
final1c <- read.csv("./pz_sims/1.5_1.0_0.001_0.01_8t_81p.csv")
final1c <- final1c[,2:4]

final2a <- read.csv("./pz_sims/1.2_0.01_0.001_15t_81p.csv")
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
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[1], bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1a[1])), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z-",y="p(z-)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(5e5,1e6))

# final1a %>% 
#   ggplot() + 
#   geom_density(aes(x=z_minus,y=after_stat(density), color = "simulation"), na.rm=TRUE) +
#   stat_function(fun = dexp, args = list(rate = as.numeric(rate1a[1])), linewidth=0.6, aes(color = "theory")) +
#   scale_colour_manual(values = c(my_palette[1], "black"),labels = c('Simulations','Theory')) 

plot1mb <- final1b %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[1], bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1b[1])), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z-",y="p(z-)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),breaks = c(5e5,1e6)) 

plot1mc <- final1c %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[1], bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1c[1])), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z-",y="p(z-)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),breaks = c(5e5,1e6))

plot1ma + plot1mb + plot1mc

# plus

plot1pa <- final1a %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[2], bins = 80, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1a[2])), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z+",y="p(z+)") + #ylim(0,0.0022) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,5e3), breaks = c(2e3,4e3)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(0,1.5e-3,3e-3))

plot1pb <- final1b %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[2], bins = 120, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1b[2])), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,2.5e4), breaks = c(1e4,2e4)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,1e-3), breaks = c(0,5e-4,1e-3)) +
  labs(x="z+",y="p(z+)")

plot1pc <- final1c %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[2], bins = 120, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1c[2])), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,2.5e4), breaks = c(1e4,2e4)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,1e-3), breaks = c(0,5e-4,1e-3)) +
  labs(x="z+",y="p(z+)") #+ ylim(0,6e-4)

plot1pa + plot1pb + plot1pc
(plot1ma + plot1mb + plot1mc) / (plot1pa + plot1pb + plot1pc)

ggsave("./first_case.png",  width = 16, height = 8, dpi = 600)

# SIZE

final1a <- final1a %>% mutate(z = z_plus + z_minus)
final1b <- final1b %>% mutate(z = z_plus + z_minus)
final1c <- final1c %>% mutate(z = z_plus + z_minus)

ratetot1a = 1./(1./as.numeric(rate1a[1]) + 1./as.numeric(rate1a[2]))
ratetot1b = 1./(1./as.numeric(rate1b[1]) + 1./as.numeric(rate1b[2]))
ratetot1c = 1./(1./as.numeric(rate1c[1]) + 1./as.numeric(rate1c[2]))

plot1a <- final1a %>% 
  ggplot(aes(x=z)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[3], bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = ratetot1a), linewidth=0.7) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z",y="p(z)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(5e5,1e6))

plot1b <- final1b %>% 
  ggplot(aes(x=z)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[3], alpha=0.8, bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = ratetot1b), linewidth=0.7) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z",y="p(z)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(5e5,1e6))

plot1c <- final1c %>% 
  ggplot(aes(x=z)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[3], alpha=0.6,bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = ratetot1c), linewidth=0.7) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z",y="p(z)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(5e5,1e6))

plot1a + plot1b + plot1c
ggsave("./first_case_size.png",  width = 16, height = 5, dpi = 600)

# JOINT
lm = 1.5; lp = 1.0; t = 8;

final1a_j = final1a %>% filter(z_plus > 1)
final1a_j = final1a_j[1:500,]
# create analytic distribution
x1a <- rexp(1000, rate = as.numeric(rate1a[1]))
y1a <- sapply(x1a, function(x) x*0.001/(lm - lp))
joint1a <- data.frame(x1a,y1a)

plot1aj <- ggplot() +
  geom_point(final1a_j,mapping = aes(x=z_minus, y=z_plus), size=0.3) +
  stat_density_2d_filled(joint1a, 
                         mapping = aes(x=x1a,y=y1a), 
                         contour_var = "ndensity", 
                         alpha = 0.4, bins = 5) +
  geom_density_2d(joint1a, mapping = aes(x=x1a,y=y1a), 
                  contour_var = "ndensity", 
                  colour = "black", bins = 5) +
  scale_x_continuous(trans = "log10",
                     limits = c(50,1.5e6),
                     breaks = c(1e2,1e4,1e6)) +
  scale_y_continuous(trans = "log10", 
                     limits = c(1,5e4),
                     labels = function(x) format(x, scientific = TRUE),
                     breaks = c(1,1e2,1e4)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20)) +
  #guides(fill=guide_legend(title="denisity")) +
  theme(legend.position="none") +
  labs(x = "z-", y = "z+")

final1b_j = final1b %>% filter(z_plus > 1)
final1b_j = final1b_j[1:500,]
# create analytic distribution
x1b <- rexp(1000, rate = as.numeric(rate1b[1]))
y1b <- sapply(x1b, function(x) x*0.005/(lm - lp))
joint1b <- data.frame(x1b,y1b)

plot1bj <- ggplot() +
  geom_point(final1b_j, mapping = aes(x=z_minus, y=z_plus), size=0.3) +
  stat_density_2d_filled(joint1b, 
                         mapping = aes(x=x1b,y=y1b), 
                         contour_var = "ndensity", alpha = 0.4, bins = 5) +
  geom_density_2d(joint1b, 
                  mapping = aes(x=x1b,y=y1b),
                  contour_var = "ndensity", 
                  colour = "black", bins = 5) +
  scale_x_continuous(trans = "log10", 
                     limits = c(50,1.5e6),
                     breaks = c(1e2,1e4,1e6)) +
  scale_y_continuous(trans = "log10",
                     limits = c(1,5e4), 
                     labels = function(x) format(x, scientific = TRUE),
                     breaks = c(1,1e2,1e4)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20)) +
  #guides(fill=guide_legend(title="density")) +
  theme(legend.position="none") +
  labs(x = "z-", y = "z+")

final1c_j = final1c %>% filter(z_plus > 1)
final1c_j = final1c_j[1:500,]
# create analytic distribution
x1c <- rexp(1000, rate = as.numeric(rate1c[1]))
y1c <- sapply(x1c, function(x) x*0.01/(lm - lp))
joint1c <- data.frame(x1c,y1c)

plot1cj <- ggplot() +
  geom_point(final1c_j,mapping = aes(x=z_minus, y=z_plus), size=0.3) +
  stat_density_2d_filled(joint1c, 
                         mapping = aes(x=x1c,y=y1c), 
                         contour_var = "ndensity", 
                         alpha = 0.4, bins = 5) +
  geom_density_2d(joint1c, 
                  mapping = aes(x=x1c,y=y1c),
                  contour_var = "ndensity", 
                  colour = "black", bins = 5) +
  scale_x_continuous(trans = "log10", 
                     limits = c(50,1.5e6),
                     breaks = c(1e2,1e4,1e6)) +
  scale_y_continuous(trans = "log10", 
                     limits = c(1,5e4),
                     labels = function(x) format(x, scientific = TRUE),
                     breaks = c(1,1e2,1e4)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20)) +
  theme(legend.position="none") +
  labs(x = "z-", y = "z+")


plot1aj + plot1bj + plot1cj
ggsave("./first_case_joint.png",  width = 16, height = 6, dpi = 600)

# CASE 2

rate2a <- Pz_exp(final2a,1.2,1.2,0.01,0.001)
rate2b <- Pz_exp(final2b,1.2,1.2,0.005,0.005)
rate2c <- Pz_exp(final2c,1.2,1.2,0.001,0.01)

# minus
plot2ma <- final2a %>% 
  #filter(`Z-` < 1e6) %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = my_palette[1], bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2a[1])), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z-",y="p(z-)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(5e4,1e5))

plot2mb <- final2b %>% 
  #filter(`Z-` < 1e6) %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = my_palette[1], bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2b[1])), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z-",y="p(z-)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(5e4,1e5), limits = c(0,1.2e5))

plot2mc <- final2c %>% 
  #filter(`Z-` < 1e6) %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = my_palette[1], bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2c[1])), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z-",y="p(z-)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(5e4,1e5), limits = c(0,1.2e5))

plot2ma + plot2mb + plot2mc

# plus
plot2pa <- final2a %>% 
  #filter(`Z+` < 1000) %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = my_palette[2], bins = 120, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2a[2])), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(breaks = c(4e3,8e3), labels = function(x) format(x, scientific = TRUE),limits = c(0,9e3)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,4e-3), breaks = c(0,2e-3,4e-3)) +
  labs(x="z+",y="p(z+)")  #+ ylim(0,0.004)


plot2pb <- final2b %>% 
  #filter(`Z+` < 1000) %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = my_palette[2], bins = 100, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2b[2])), size=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(breaks = c(4e3,8e3), labels = function(x) format(x, scientific = TRUE),limits = c(0,9e3)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,4e-3), breaks = c(0,2e-3,4e-3)) +
  labs(x="z+",y="p(z+)") #+ ylim(0,0.004)

plot2pc <- final2c %>% 
  #filter(`Z+` < 1000) %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = my_palette[2], bins = 100, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2c[2])), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(breaks = c(4e3,8e3), labels = function(x) format(x, scientific = TRUE),limits = c(0,9e3)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,4e-3), breaks = c(0,2e-3,4e-3)) +
  labs(x="z+",y="p(z+)") #+ ylim(0,0.004)

plot2pa + plot2pb + plot2pc
(plot2ma + plot2mb + plot2mc) / (plot2pa + plot2pb + plot2pc)

ggsave("./second_case.png",  width = 16, height = 8, dpi = 600)

# SIZE
final2a <- final2a %>% mutate(z = z_plus + z_minus)
final2b <- final2b %>% mutate(z = z_plus + z_minus)
final2c <- final2c %>% mutate(z = z_plus + z_minus)

ratetot2a = 1./(1./as.numeric(rate2a[1]) + 1./as.numeric(rate2a[2]))
ratetot2b = 1./(1./as.numeric(rate2b[1]) + 1./as.numeric(rate2b[2]))
ratetot2c = 1./(1./as.numeric(rate2c[1]) + 1./as.numeric(rate2c[2]))

plot2a <- final2a %>% 
  ggplot(aes(x=z)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[3], bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = ratetot2a), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z",y="p(z)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,1.2e5), breaks = c(5e4,1e5))

plot2b <- final2b %>% 
  ggplot(aes(x=z)) + 
  geom_histogram(aes(y=after_stat(density)), fill = "#9581BC", bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = ratetot2b), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z",y="p(z)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,1.2e5), breaks = c(5e4,1e5))

plot2c <- final2c %>% 
  ggplot(aes(x=z)) + 
  geom_histogram(aes(y=after_stat(density)), fill = "#AA9BCB", bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = ratetot2c), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z",y="p(z)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,1.2e5), breaks = c(5e4,1e5))

plot2a + plot2b + plot2c
ggsave("./second_case_size.png",  width = 16, height = 5, dpi = 600)

# JOINT
final2a_j = final2a %>% filter(z_plus > 1)
final2a_j = final2a_j[1:800,]
# create analytic distribution
x2a <- rexp(1000, rate = as.numeric(rate2a[1]))
y2a <- sapply(x2a, function(x) x*as.numeric(rate2a[1])/as.numeric(rate2a[2]))
joint2a <- data.frame(x2a,y2a)

plot2aj <- ggplot() +
  geom_point(final2a_j,mapping = aes(x=z_minus, y=z_plus), size=0.3) +
  stat_density_2d_filled(joint2a, 
                         mapping = aes(x=x2a,y=y2a), 
                         contour_var = "ndensity", 
                         alpha = 0.4, bins = 5) +
  geom_density_2d(joint2a, mapping = aes(x=x2a,y=y2a), 
                  contour_var = "ndensity", 
                  colour = "black", bins = 5) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10",
                     labels = function(x) format(x, scientific = TRUE)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20)) +
  guides(fill=guide_legend(title="density")) +
  #theme(legend.position="none") +
  labs(x = "z-", y = "z+")

final2b_j = final2b %>% filter(z_plus > 1)
final2b_j = final2b_j[1:500,]
# create analytic distribution
x2b <- rexp(1000, rate = exp(-1.2*t)/cosh(sqrt(0.005*0.005)*8))
y2b <- sapply(x2b, function(x) x*tanh(sqrt(0.005*0.005)*8))
joint2b <- data.frame(x2b,y2b)

plot2bj <- ggplot() +
  geom_point(final2b_j, mapping = aes(x=z_minus, y=z_plus), size=0.3) +
  stat_density_2d_filled(joint2b, 
                         mapping = aes(x=x2b,y=y2b), 
                         contour_var = "ndensity", alpha = 0.4, bins = 5) +
  geom_density_2d(joint2b, 
                  mapping = aes(x=x2b,y=y2b),
                  contour_var = "ndensity", 
                  colour = "black", bins = 5) +
  scale_x_continuous(trans = "log10", 
                     limits = c(50,1.5e6),
                     breaks = c(1e2,1e4,1e6)) +
  scale_y_continuous(trans = "log10",
                     limits = c(1,5e4), 
                     labels = function(x) format(x, scientific = TRUE),
                     breaks = c(1,1e2,1e4)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20)) +
  #guides(fill=guide_legend(title="density")) +
  theme(legend.position="none") +
  labs(x = "z-", y = "z+")

final3c_j = final3c %>% filter(z_plus > 1)
final3c_j = final3c_j[1:500,]
# create analytic distribution
x3c <- rexp(1000, rate = exp(-1.2*t)/cosh(sqrt(0.01*0.001)*8))
y3c <- sapply(x3c, function(x) x*sqrt(0.01/0.001)*tanh(sqrt(0.01*0.001)*8))
joint3c <- data.frame(x3c,y3c)

plot3cj <- ggplot() +
  geom_point(final3c_j,mapping = aes(x=z_minus, y=z_plus), size=0.3) +
  stat_density_2d_filled(joint3c, 
                         mapping = aes(x=x3c,y=y3c), 
                         contour_var = "ndensity", 
                         alpha = 0.4, bins = 5) +
  geom_density_2d(joint3c, 
                  mapping = aes(x=x3c,y=y3c),
                  contour_var = "ndensity", 
                  colour = "black", bins = 5) +
  scale_x_continuous(trans = "log10", 
                     limits = c(50,1.5e6),
                     breaks = c(1e2,1e4,1e6)) +
  scale_y_continuous(trans = "log10", 
                     limits = c(1,5e4),
                     labels = function(x) format(x, scientific = TRUE),
                     breaks = c(1,1e2,1e4)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20)) +
  theme(legend.position="none") +
  labs(x = "z-", y = "z+")


plot1aj + plot1bj + plot1cj
ggsave("./first_case_joint.png",  width = 16, height = 6, dpi = 600)

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
  geom_histogram(aes(y=after_stat(density)), bins=80, fill=my_palette[1], na.rm = TRUE) +
  geom_line(data=analytic1 %>% filter(type=="z_minus"), aes(x=z,y=P), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(breaks = c(1e4,2e4), labels = function(x) format(x, scientific = TRUE),limits = c(0,2.5e4)) +
  labs(x="z-",y="p(z-)")
plot3ma

plot3mb <- final3b %>% 
  ggplot(aes(x=z_minus)) +
  geom_histogram(aes(y=after_stat(density)), bins=80, fill=my_palette[1], na.rm = TRUE) +
  geom_line(data=analytic2 %>% filter(type=="z_minus"), aes(x=z,y=P), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(breaks = c(1e4,2e4), labels = function(x) format(x, scientific = TRUE),limits = c(0,2.5e4)) +
  labs(x="z-",y="p(z-)")

plot3mc <- final3c %>% 
  ggplot(aes(x=z_minus)) +
  geom_histogram(aes(y=after_stat(density)), bins=80, fill=my_palette[1], na.rm = TRUE) +
  geom_line(data=analytic3 %>% filter(type=="z_minus"), aes(x=z,y=P), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(breaks = c(1e4,2e4), labels = function(x) format(x, scientific = TRUE),limits = c(0,2.5e4)) +
  labs(x="z-",y="p(z-)")

(plot3ma + plot3mb + plot3mc)

# plus

plot3pa <- final3a %>% 
  ggplot(aes(x=z_plus)) +
  geom_histogram(aes(y=after_stat(density)), bins=150, fill=my_palette[2], na.rm = TRUE) +
  geom_line(data=analytic1 %>% filter(type=="z_plus"), aes(x=z,y=P), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(breaks = c(2e4,4e4), labels = function(x) format(x, scientific = TRUE),limits = c(0,5e4)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),limits = c(0,2e-4)) +
  labs(x="z+",y="p(z+)")
plot3pa

plot3pb <- final3b %>% 
  ggplot(aes(x=z_plus)) +
  geom_histogram(aes(y=after_stat(density)), bins=150, fill=my_palette[2], na.rm = TRUE) +
  geom_line(data=analytic2 %>% filter(type=="z_plus"), aes(x=z,y=P), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(breaks = c(2e4,4e4), labels = function(x) format(x, scientific = TRUE),limits = c(0,5e4)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),limits = c(0,2e-4)) +
  labs(x="z+",y="p(z+)")

plot3pc <- final3c %>% 
  ggplot(aes(x=z_plus)) +
  geom_histogram(aes(y=after_stat(density)), bins=150, fill=my_palette[2], na.rm = TRUE) +
  geom_line(data=analytic3 %>% filter(type=="z_plus"), aes(x=z,y=P), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(breaks = c(2e4,4e4), labels = function(x) format(x, scientific = TRUE),limits = c(0,5e4)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),limits = c(0,2e-4)) +
  labs(x="z+",y="p(z+)")

plot3pa + plot3pb + plot3pc
(plot3ma + plot3mb + plot3mc) / (plot3pa + plot3pb + plot3pc)
ggsave("./third_case.png",  width = 16, height = 8, dpi = 600)

# size
final3a <- final3a %>% mutate(z = z_plus + z_minus)
final3b <- final3b %>% mutate(z = z_plus + z_minus)
final3c <- final3c %>% mutate(z = z_plus + z_minus)

analytic1tot <- read.csv("1_1.5_0.01_0.001_8t_tot.csv") %>% tibble::as_tibble()
analytic2tot <- read.csv("1_1.5_0.005_8t_tot.csv") %>% tibble::as_tibble()
analytic3tot <- read.csv("1_1.5_0.001_0.01_8t_tot.csv") %>% tibble::as_tibble()

plot3a <- final3a %>% 
  ggplot(aes(x=z)) +
  geom_histogram(aes(y=after_stat(density)), bins=150, fill=my_palette[3], na.rm = TRUE) +
  geom_line(data=analytic1tot, aes(x=z,y=p), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(2e4,4e4)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  labs(x="z",y="p(z)")

plot3b <- final3b %>% 
  ggplot(aes(x=z)) +
  geom_histogram(aes(y=after_stat(density)), bins=150, fill="#9581BC", na.rm = TRUE) +
  geom_line(data=analytic2tot, aes(x=z,y=p), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,9e4), breaks = c(4e4,8e4)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  labs(x="z",y="p(z)")

plot3c <- final3c %>% 
  ggplot(aes(x=z)) +
  geom_histogram(aes(y=after_stat(density)), bins=150, fill="#AA9BCB", na.rm = TRUE) +
  geom_line(data=analytic3tot, aes(x=z,y=p), linewidth=0.6) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,9e4), breaks = c(4e4,8e4)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,3e-4)) +
  labs(x="z",y="p(z)")

plot3a + plot3b + plot3c
ggsave("./third_case_size.png",  width = 16, height = 5, dpi = 600)
