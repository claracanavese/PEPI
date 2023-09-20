library(ggplot2)
library(magrittr)
library(tidyverse)
library(reshape2)
library(patchwork)
library(plotly)
library(stabledist)
library(pdqr)

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
my_palette <- c("#72B8B5","#FFCA0A","#265450")

# upload simulated distributions

final1a <- read.csv("./python/1.5_1.0_0.01_0.001_10t_51p.csv")
final1a <- final1a[,2:4]
final1b <- read.csv("./python/1.5_1.0_0.005_10t_51p.csv")
final1b <- final1b[,2:4]
final1c <- read.csv("./python/1.5_1.0_0.001_0.01_10t_51p.csv")
final1c <- final1c[,2:4]

final2a <- read.csv("./python/1.2_0.01_0.001_15t_51p.csv")
final2a <- final2a[,2:4]
final2b <- read.csv("./python/1.2_0.005_15t_51p.csv")
final2b <- final2b[,2:4]
final2c <- read.csv("./python/1.2_0.001_0.01_15t_51p.csv")
final2c <- final2c[,2:4]

final3a <- read.csv("./python/1.0_1.5_0.01_0.001_15t_51p.csv")
final3a <- final3a[,2:4]
final3b <- read.csv("./python/1.0_1.5_0.005_15t_51p.csv")
final3b <- final3b[,2:4]
final3c <- read.csv("./python/1.0_1.5_0.001_0.01_15t_51p.csv")
final3c <- final3c[,2:4]

# CASE 1

rate1a <- Pz_exp(final1a,1.5,1.0,0.01,0.001)
rate1b <- Pz_exp(final1b,1.5,1.0,0.005,0.005)
rate1c <- Pz_exp(final1c,1.5,1.0,0.001,0.01)

ggplot(final1a) +
  geom_histogram(aes(x=z_minus, y=after_stat(density), fill = "Z-"), bins = 70, na.rm=TRUE) +
  geom_histogram(aes(x=z_plus, y=after_stat(density), fill = "Z+"), bins = 70, na.rm=TRUE) +
  scale_fill_manual(values = my_palette) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 16))
ggsave("./legend_marginals.png",dpi=600)

# minus
plot1ma <- final1a %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[1], bins = 70, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1a[1])), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z-",y="p(z-)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), 
                     limits = c(0,3e7)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     breaks = c(0,2e-7,4e-7), limits = c(0,4e-7))

# final1a %>% 
#   ggplot() + 
#   geom_density(aes(x=z_minus,y=after_stat(density), color = "simulation"), na.rm=TRUE) +
#   stat_function(fun = dexp, args = list(rate = as.numeric(rate1a[1])), linewidth=0.6, aes(color = "theory")) +
#   scale_colour_manual(values = c(my_palette[1], "black"),labels = c('Simulations','Theory')) 

plot1mb <- final1b %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[1], bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1b[1])), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z-",y="p(z-)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,3e7)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     breaks = c(0,2e-7,4e-7), limits = c(0,4e-7))

plot1mc <- final1c %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[1], bins = 90, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1c[1])), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z-",y="p(z-)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,3e7)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     breaks = c(0,2e-7,4e-7), limits = c(0,4e-7))

plot1ma / plot1mb / plot1mc
ggsave("./pz/pmz_1_10t.png",  width = 5, height = 10, dpi = 600)

# plus
plot1pa <- final1a %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[2], bins = 90, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1a[2])), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z+",y="p(z+)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), 
                     limits = c(0,3e5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,4e-5),
                     breaks = c(0,2e-5,4e-5))

plot1pb <- final1b %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[2], bins = 80, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1b[2])), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z+",y="p(z+)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), 
                     limits = c(0,3e5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,4e-5),
                     breaks = c(0,2e-5,4e-5))

plot1pc <- final1c %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[2], bins = 70, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate1c[2])), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z+",y="p(z+)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), 
                     limits = c(0,3e5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,4e-5),
                     breaks = c(0,2e-5,4e-5))

plot1pa / plot1pb / plot1pc
ggsave("./pz/ppz_1_10t.png",  width = 5, height = 10, dpi = 600)
(plot1ma + plot1mb + plot1mc) / (plot1pa + plot1pb + plot1pc)

ggsave("./first_case.png",  width = 16, height = 8, dpi = 600)

# SIZE
final1a <- final1a %>% mutate(z = z_minus + z_plus)
final1b <- final1b %>% mutate(z = z_plus + z_minus)
final1c <- final1c %>% mutate(z = z_plus + z_minus)

ratetot1a = 1./(1./as.numeric(rate1a[1]) + 1./as.numeric(rate1a[2]))
ratetot1b = 1./(1./as.numeric(rate1b[1]) + 1./as.numeric(rate1b[2]))
ratetot1c = 1./(1./as.numeric(rate1c[1]) + 1./as.numeric(rate1c[2]))

plot1a <- final1a %>% 
  ggplot(aes(x=z)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[3], bins = 80, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = ratetot1a), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z",y="p(z)") + 
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(1e7,2e7),limits = c(0,2.75e7))

plot1b <- final1b %>% 
  ggplot(aes(x=z)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[3], alpha=0.8, bins = 80, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = ratetot1b), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z",y="p(z)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(1e7,2e7))

plot1c <- final1c %>% 
  ggplot(aes(x=z)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[3], alpha=0.6,bins = 80, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = ratetot1c), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z",y="p(z)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(1e7,2e7), limits = c(0,2.75e7))

plot1a / plot1b / plot1c
ggsave("./pz/ptz_1_10t.png",  width = 5, height = 10, dpi = 600)
ggsave("./first_case_size.png",  width = 16, height = 5, dpi = 600)

# JOINT
final1a_j = final1a %>% filter(z_plus > 10)
# create analytic distribution
x1a <- rexp(1000, rate = as.numeric(rate1a[1]))
y1a <- sapply(x1a, function(x) x*as.numeric(rate1a[1])/as.numeric(rate1a[2]))
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
                     limits = c(1e3,1e8)) +
                     #breaks = c(1e2,1e4,1e6)) +
  scale_y_continuous(trans = "log10",
                     limits = c(1e1,5e5),
                     labels = function(x) format(x, scientific = TRUE),
                     breaks = c(1e1,1e3,1e5)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20)) +
  #guides(fill=guide_legend(title="density")) +
  #theme(legend.position="none") +
  labs(x = "z-", y = "z+")

final1b_j = final1b %>% filter(z_plus > 10)
final1b_j = final1b_j[1:500,]
# create analytic distribution
x1b <- rexp(1000, rate = as.numeric(rate1b[1]))
y1b <- sapply(x1b, function(x) x*as.numeric(rate1b[1])/as.numeric(rate1b[2]))
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
                     limits = c(1e3,1e8)) +
                     #breaks = c(1e2,1e4,1e6)) +
  scale_y_continuous(trans = "log10",
                     limits = c(1e1,5e5), 
                     labels = function(x) format(x, scientific = TRUE),
                     breaks = c(1e1,1e3,1e5)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20)) +
  #guides(fill=guide_legend(title="density")) +
  #theme(legend.position="none") +
  labs(x = "z-", y = "z+")

final1c_j = final1c %>% filter(z_plus > 10)
final1c_j = final1c_j[1:500,]
# create analytic distribution
x1c <- rexp(1000, rate = as.numeric(rate1c[1]))
y1c <- sapply(x1c, function(x) x*as.numeric(rate1c[1])/as.numeric(rate1c[2]))
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
                     limits = c(1e3,1e8)) +
                     #breaks = c(1e2,1e4,1e6)) +
  scale_y_continuous(trans = "log10", 
                     limits = c(1e1,5e5),
                     labels = function(x) format(x, scientific = TRUE),
                     breaks = c(1e1,1e3,1e5)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20)) +
  #theme(legend.position="none") +
  labs(x = "z-", y = "z+")


plot1aj / plot1bj / plot1cj
ggsave("./pz/pjz_1_10t.png",  width = 5, height = 10, dpi = 600)
ggsave("./first_case_joint.png",  width = 16, height = 6, dpi = 600)

# CASE 2
rate2a <- Pz_exp(final2a,1.2,1.2,0.01,0.001)
rate2b <- Pz_exp(final2b,1.2,1.2,0.005,0.005)
rate2c <- Pz_exp(final2c,1.2,1.2,0.001,0.01)

# minus
plot2ma <- final2a %>% 
  #filter(`Z-` < 1e6) %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = my_palette[1], bins = 80, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2a[1])), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z-",y="p(z-)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), 
                     #limits = c(0,1e6),
                     breaks = c(0,2e8,4e8)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), 
                     limits = c(0,2e-8),
                     breaks = c(0,1e-8,2e-8))

plot2mb <- final2b %>% 
  #filter(`Z-` < 1e6) %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = my_palette[1], bins = 80, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2b[1])), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z-",y="p(z-)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), 
                     #limits = c(0,1e6),
                     breaks = c(0,2e8,4e8)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), 
                     limits = c(0,2e-8),
                     breaks = c(0,1e-8,2e-8))

plot2mc <- final2c %>% 
  #filter(`Z-` < 1e6) %>% 
  ggplot(aes(x=z_minus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = my_palette[1], bins = 80, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2c[1])), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z-",y="p(z-)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), 
                     #limits = c(0,1e6),
                     breaks = c(0,2e8,4e8)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), 
                     limits = c(0,2e-8),
                     breaks = c(0,1e-8,2e-8))

plot2ma / plot2mb / plot2mc
ggsave("./pz/pmz_2_15t.png",  width = 5, height = 10, dpi = 600)

# plus
plot2pa <- final2a %>% 
  #filter(`Z+` < 1000) %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = my_palette[2], bins = 90, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2a[2])), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),limits = c(0,3.5e7)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), breaks = c(0,5e-7,1e-6)) +
  labs(x="z+",y="p(z+)") 

plot2pb <- final2b %>% 
  #filter(`Z+` < 1000) %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = my_palette[2], bins = 90, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2b[2])), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),limits = c(0,3.5e7)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,3e-7)) +
  labs(x="z+",y="p(z+)")

plot2pc <- final2c %>% 
  #filter(`Z+` < 1000) %>% 
  ggplot(aes(x=z_plus)) + 
  geom_histogram(aes(y=after_stat(density)),fill = my_palette[2], bins = 90, na.rm = TRUE) +
  stat_function(fun = dexp, args = list(rate = as.numeric(rate2c[2])), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),limits = c(0,3.5e7)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,3e-7)) +
  labs(x="z+",y="p(z+)") #+ ylim(0,0.004)

plot2pa / plot2pb / plot2pc
ggsave("./pz/ppz_2_15t.png",  width = 5, height = 10, dpi = 600)

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
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[3], alpha = 0.9, bins = 80, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = ratetot2a), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z",y="p(z)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,4.5e8), breaks = c(0,2e8,4e8)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,1.6e-8), breaks = c(0,6e-9,1.2e-8))

plot2b <- final2b %>% 
  ggplot(aes(x=z)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[3], alpha = 0.7, bins = 80, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = ratetot2b), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z",y="p(z)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,4.5e8), breaks = c(0,2e8,4e8)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,1.6e-8), breaks = c(0,6e-9,1.2e-8))

plot2c <- final2c %>% 
  ggplot(aes(x=z)) + 
  geom_histogram(aes(y=after_stat(density)), fill = my_palette[3], alpha = 0.5, bins = 80, na.rm=TRUE) +
  stat_function(fun = dexp, args = list(rate = ratetot2c), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  labs(x="z",y="p(z)") +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,4.5e8), breaks = c(0,2e8,4e8)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,1.6e-8), breaks = c(0,6e-9,1.2e-8))

plot2a / plot2b / plot2c
ggsave("./pz/ptz_2_15t.png",  width = 5, height = 10, dpi = 600)
ggsave("./second_case_size.png",  width = 16, height = 5, dpi = 600)

# JOINT
final2a_j = final2a %>% filter(z_plus > 10)
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
  scale_x_continuous(trans = "log10",
                     limits = c(5e4,6e8),
                     breaks = c(1e6,1e8)) +
  scale_y_continuous(trans = "log10",
                     labels = function(x) format(x, scientific = TRUE),
                     limits = c(1e3,1e8)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20)) +
  guides(fill=guide_legend(title="density")) +
  #theme(legend.position="none") +
  labs(x = "z-", y = "z+")

final2b_j = final2b %>% filter(z_plus > 10)
final2b_j = final2b_j[1:500,]
# create analytic distribution
x2b <- rexp(1000, rate = as.numeric(rate2b[1]))
y2b <- sapply(x2b, function(x) x*as.numeric(rate2b[1])/as.numeric(rate2b[2]))
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
                     limits = c(5e4,6e8),
                     breaks = c(1e6,1e8)) +
  scale_y_continuous(trans = "log10", 
                     labels = function(x) format(x, scientific = TRUE),
                     limits = c(1e3,1e8)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20)) +
  guides(fill=guide_legend(title="density")) +
  #theme(legend.position="none") +
  labs(x = "z-", y = "z+")

final2c_j = final2c %>% filter(z_plus > 10)
final2c_j = final2c_j[1:500,]
# create analytic distribution
x2c <- rexp(1000, rate = as.numeric(rate2c[1]))
y2c <- sapply(x2c, function(x) x*as.numeric(rate2c[1])/as.numeric(rate2c[2]))
joint2c <- data.frame(x2c,y2c)

plot2cj <- ggplot() +
  geom_point(final2c_j,mapping = aes(x=z_minus, y=z_plus), size=0.3) +
  stat_density_2d_filled(joint2c, 
                         mapping = aes(x=x2c,y=y2c), 
                         contour_var = "ndensity", 
                         alpha = 0.4, bins = 5) +
  geom_density_2d(joint2c, 
                  mapping = aes(x=x2c,y=y2c),
                  contour_var = "ndensity", 
                  colour = "black", bins = 5) +
  scale_x_continuous(trans = "log10",
                     limits = c(5e4,6e8),
                     breaks = c(1e6,1e8)) +
  scale_y_continuous(trans = "log10", 
                     labels = function(x) format(x, scientific = TRUE),
                     limits = c(1e3,1e8)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 20)) +
  #theme(legend.position="none") +
  labs(x = "z-", y = "z+")


plot2aj / plot2bj / plot2cj
ggsave("./pz/pjz_2_15t.png",  width = 5, height = 10, dpi = 600)

# CASE 3

analytic1 <- read.csv("./PEPI/analytics/1_1.5_0.01_0.001_15t_minus.csv") %>%
  tibble::as_tibble() %>% 
  mutate(type="z_minus") %>% 
  add_row(read.csv("./PEPI/analytics/1_1.5_0.01_0.001_15t_plus.csv") %>% 
            tibble::as_tibble() %>% 
            mutate(type="z_plus")) 

analytic2 <- read.csv("./PEPI/analytics/1_1.5_0.005_15t_minus.csv") %>%
  tibble::as_tibble() %>% 
  mutate(type="z_minus") %>% 
  add_row(read.csv("./PEPI/analytics/1_1.5_0.005_15t_plus.csv") %>% 
            tibble::as_tibble() %>% 
            mutate(type="z_plus"))

analytic3 <- read.csv("./PEPI/analytics/1_1.5_0.001_0.01_15t_minus.csv") %>%
  tibble::as_tibble() %>% 
  mutate(type="z_minus") %>% 
  add_row(read.csv("./PEPI/analytics/1_1.5_0.001_0.01_15t_plus.csv") %>% 
            tibble::as_tibble() %>% 
            mutate(type="z_plus"))

# minus
plot3ma <- final3a %>% 
  ggplot(aes(x=z_minus)) +
  geom_histogram(aes(y=after_stat(density)), bins=90, fill=my_palette[1], na.rm = TRUE) +
  geom_line(data=analytic1 %>% filter(type=="z_minus"), aes(x=z,y=P), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,3e7)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     breaks = c(0,1.5e-7,3e-7)) +
  labs(x="z-",y="p(z-)")
plot3ma

plot3mb <- final3b %>% 
  ggplot(aes(x=z_minus)) +
  geom_histogram(aes(y=after_stat(density)), bins=80, fill=my_palette[1], na.rm = TRUE) +
  geom_line(data=analytic2 %>% filter(type=="z_minus"), aes(x=z,y=P), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,3e7)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     breaks = c(0,1.5e-7,3e-7)) +
  labs(x="z-",y="p(z-)")

plot3mc <- final3c %>% 
  ggplot(aes(x=z_minus)) +
  geom_histogram(aes(y=after_stat(density)), bins=100, fill=my_palette[1], na.rm = TRUE) +
  geom_line(data=analytic3 %>% filter(type=="z_minus"), aes(x=z,y=P), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,3e7)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     breaks = c(0,1.5e-7,3e-7)) +
  labs(x="z-",y="p(z-)")

plot3ma / plot3mb / plot3mc
ggsave("./pz/pmz_3_15t.png",  width = 5, height = 10, dpi = 600)

# plus
plot3pa <- final3a %>% 
  ggplot(aes(x=z_plus)) +
  geom_histogram(aes(y=after_stat(density)), bins = 100, fill=my_palette[2], na.rm = TRUE) + 
  geom_line(data=analytic1 %>% filter(type=="z_plus"), aes(x=z,y=P), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,7e7)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,2e-7),
                     breaks = c(0,1e-7,2e-7)) +
  labs(x="z+",y="p(z+)")
plot3pa

plot3pb <- final3b %>% 
  ggplot(aes(x=z_plus)) +
  geom_histogram(aes(y=after_stat(density)), bins=90, fill=my_palette[2], na.rm = TRUE) +
  geom_line(data=analytic2 %>% filter(type=="z_plus"), aes(x=z,y=P), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,7e7)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,2e-7),
                     breaks = c(0,1e-7,2e-7)) +
  labs(x="z+",y="p(z+)")

plot3pc <- final3c %>% 
  ggplot(aes(x=z_plus)) +
  geom_histogram(aes(y=after_stat(density)), bins=90, fill=my_palette[2], na.rm = TRUE) +
  geom_line(data=analytic3 %>% filter(type=="z_plus"), aes(x=z,y=P), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,7e7)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,2e-7),
                     breaks = c(0,1e-7,2e-7)) +
  labs(x="z+",y="p(z+)")

plot3pa / plot3pb / plot3pc
ggsave("./pz/ppz_3_15t.png",  width = 5, height = 10, dpi = 600)
ggsave("./third_case.png",  width = 16, height = 8, dpi = 600)

# size
final3a <- final3a %>% mutate(z = z_plus + z_minus)
final3b <- final3b %>% mutate(z = z_plus + z_minus)
final3c <- final3c %>% mutate(z = z_plus + z_minus)

analytic1tot <- read.csv("./PEPI/analytics/1_1.5_0.01_0.001_15t_tot.csv") %>% tibble::as_tibble()
analytic2tot <- read.csv("./PEPI/analytics/1_1.5_0.005_15t_tot.csv") %>% tibble::as_tibble()
analytic3tot <- read.csv("./PEPI/analytics/1_1.5_0.001_0.01_15t_tot.csv") %>% tibble::as_tibble()

plot3a <- final3a %>% 
  ggplot(aes(x=z)) +
  geom_histogram(aes(y=after_stat(density)), bins=100, fill=my_palette[3], alpha=0.9, na.rm = TRUE) +
  geom_line(data=analytic1tot, aes(x=z,y=P), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), 
                     limits = c(0,1e8),
                     breaks = c(0,3e7,6e7,9e7)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,2e-7),
                     breaks = c(0,1e-7,2e-7)) +
  labs(x="z",y="p(z)")

plot3b <- final3b %>% 
  ggplot(aes(x=z)) +
  geom_histogram(aes(y=after_stat(density)), bins=100, fill=my_palette[3], alpha=0.7, na.rm = TRUE) + 
  geom_line(data=analytic2tot, aes(x=z,y=P), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), 
                     limits = c(0,1e8),
                     breaks = c(0,3e7,6e7,9e7)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,2e-7),
                     breaks = c(0,1e-7,2e-7)) +
  labs(x="z",y="p(z)")

plot3c <- final3c %>% 
  ggplot(aes(x=z)) +
  geom_histogram(aes(y=after_stat(density)), bins=100,fill=my_palette[3], alpha=0.5, na.rm = TRUE) +
  geom_line(data=analytic3tot, aes(x=z,y=P), linewidth=1) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), 
                     limits = c(0,1e8),
                     breaks = c(0,3e7,6e7,9e7)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE),
                     limits = c(0,2e-7),
                     breaks = c(0,1e-7,2e-7)) +
  labs(x="z",y="p(z)")

plot3a / plot3b / plot3c
ggsave("./pz/ptz_3_15t.png",  width = 5, height = 10, dpi = 600)
ggsave("./third_case_size.png",  width = 16, height = 5, dpi = 600)

# JOINT
powerlaw_coeff_plus <- function(am,ap,lm,lp,om,op,t) {
  return(am*op*pi*exp((lm+om*op/(lp-lm)*lm/lp)*t)/(lp*ap*sin(pi*lm/lp)*gamma(1-lm/lp)))
}

powerlaw_coeff_minus <- function(am,ap,lm,lp,om,op,t) {
  return(am*op*pi*(om/(lp-lm))^(lm/lp)*exp((lm+om*op/(lp-lm)*lm/lp)*t)/(lp*ap*sin(pi*lm/lp)*gamma(1-lm/lp)))
}

powerlaw_coeff1m <- powerlaw_coeff_minus(am = 1.0,ap = 1.5,lm = 1.0, lp = 1.5, om = 0.01,op = 0.001, t = mean(final3a$time))
powerlaw_coeff1p <- powerlaw_coeff_plus(am = 1.0,ap = 1.5,lm = 1.0, lp = 1.5, om = 0.01,op = 0.001, t = mean(final3a$time))

powerlaw <- function(x,coeff,exponent) {
  return(coeff*x^exponent)
}

support <- seq(1e4, 1e10, 1e5)
probs <- powerlaw(support, powerlaw_coeff1p, -1-1/1.5)
probs <- probs/sum(probs) 
sim_pl <- data.frame("z" = support, "p" = probs)
ggplot(sim_pl) +
  geom_line(aes(x=z,y=p)) + xlim(0,1e8) + ylim(0,1e-6)

colnames(sim_pl) = c("x","y")
new_r(sim_pl, type = "continuous")(1000)

plot(support, probs, type = "l")

final3a %>% 
  mutate(pw=powerlaw_coeff1m*z_minus^(-1-1.0/1.5)) %>%
  ggplot(aes(x=z_minus)) +
  geom_histogram(aes(y=after_stat(density)), bins=150, fill=my_palette[1], na.rm = TRUE) +
  stat_function(fun = powerlaw, args = list(coeff = powerlaw_coeff1m, exponent = -1-1/1.5)) +
  #geom_line(aes(y=pw), color='black') +
  xlim(0,150666402) + ylim(0,2.5e-7)

final3a %>% 
  mutate(pw=powerlaw_coeff1p*z_plus^(-1-1.0/1.5)) %>%
  ggplot(aes(x=z_plus)) +
  geom_histogram(aes(y=after_stat(density)), bins=150, fill=my_palette[2], na.rm = TRUE) +
  #geom_line(aes(y=pw), color='black') +
  stat_function(fun = powerlaw, args = list(coeff = powerlaw_coeff1p, exponent = -1-1/1.5)) +
  xlim(0,2e8) + ylim(0,2e-7)

# create analytic distribution
y3a <- new_r(sim_pl, type = "continuous")(1000)
x3a <- sapply(y3a, function(x) x*0.5/0.01)
joint3a <- data.frame(x3a,y3a)

final3a_j = final3a %>% filter(z_plus < 1e8 & z_minus < 3e7)
ggplot() +
  geom_point(final3a_j,mapping = aes(x=z_minus, y=z_plus), size=0.3) +
  stat_density_2d_filled(joint3a, 
                         mapping = aes(x=x3a,y=y3a), 
                         contour_var = "ndensity", 
                         alpha = 0.4, bins = 8) +
  geom_density_2d(joint3a, mapping = aes(x=x3a,y=y3a), 
                  contour_var = "ndensity", 
                  colour = "black", bins = 8) +
  scale_x_continuous(trans = "log10",
                     limits = c(5e4,2e7)) +
  scale_y_continuous(trans = "log10", 
                     labels = function(x) format(x, scientific = TRUE),
                     limits = c(1e3,1e6))

ggplot() +
  geom_point(final3a_j,mapping = aes(x=z_minus, y=z_plus), size=0.3) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10", 
                     labels = function(x) format(x, scientific = TRUE))
