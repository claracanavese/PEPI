library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(patchwork)

# CUMULATIVE DISTRIBUTION

gk <- function(t,k,l) {
  return(t^k*exp(l*t)/l + (-1)^k*factorial(k)/l^(k+1)*(exp(l*t)-1))
}

# Case 1
c1odd <- function(t,k,om,op,lm,lp) {
  return(op*(om*op/(lm-lp))^k*gk(t,k,lm)/factorial(k))
}
c1even <- function(t,k,om,op,lm,lp) {
  return((om*op/(lm-lp))^k*gk(t,k-1,lm)/factorial(k-1))
}

cdf1 <- function(t,n,om,op,lm,lp) {
  if(n %% 2 != 0) {
    k = (n-1)/2
    return(1.0/(c1odd(t,k,om,op,lm,lp)+1.0))
  }
  else if(n %% 2 == 0) {
    k = n/2
    return(1.0/(c1even(t,k,om,op,lm,lp)+1.0))
  }
}

ggplot() +
  geom_function(fun = cdf1, args = list(n=1,om=0.01,op=0.001,lm=1.5,lp=1.0), aes(color = "1")) +
  geom_function(fun = cdf1, args = list(n=2,om=0.01,op=0.001,lm=1.5,lp=1.0), aes(color = "2")) +
  geom_function(fun = cdf1, args = list(n=3,om=0.01,op=0.001,lm=1.5,lp=1.0), aes(color = "3")) +
  xlim(0,15) + ylim(0,1)

ggplot() +
  geom_function(fun = cdf1, args = list(n=1,om=0.01,op=0.001,lm=1.0,lp=1.5), aes(color = "0.01")) +
  geom_function(fun = cdf1, args = list(n=1,om=0.005,op=0.005,lm=1.0,lp=1.5), aes(color = "0.005")) +
  geom_function(fun = cdf1, args = list(n=1,om=0.001,op=0.01,lm=1.0,lp=1.5), aes(color = "0.001")) +
  xlim(0,15) + ylim(0,1)

# Case 2
c2odd <- function(t,k,om,op,l) {
  return(op*(om*op)^k*gk(t,2*k,l)/factorial(2*k))
}
c2even <- function(t,k,om,op,l) {
  return((om*op)^k*gk(t,2*k-1,l)/factorial(2*k-1))
}

cdf2 <- function(t,n,om,op,l) {
  if(n %% 2 != 0) {
    k = (n-1)/2
    return(1.0/(c2odd(t,k,om,op,l)+1.0))
  }
  else if(n %% 2 == 0) {
    k = n/2
    return(1.0/(c2even(t,k,om,op,l)+1.0))
  }
}

ggplot() +
  geom_function(fun = cdf2, args = list(n=1,om=0.01,op=0.001,l=1.2), aes(color = "1")) +
  geom_function(fun = cdf2, args = list(n=2,om=0.01,op=0.001,l=1.2), aes(color = "2")) +
  geom_function(fun = cdf2, args = list(n=3,om=0.01,op=0.001,l=1.2), aes(color = "3")) +
  xlim(0,15) + ylim(0,1)

ggplot() +
  geom_function(fun = cdf2, args = list(n=1,om=0.01,op=0.001,l=1.2), aes(color = "0.01")) +
  geom_function(fun = cdf2, args = list(n=1,om=0.005,op=0.005,l=1.2), aes(color = "0.005")) +
  geom_function(fun = cdf2, args = list(n=1,om=0.001,op=0.01,l=1.2), aes(color = "0.001")) +
  xlim(0,15) + ylim(0,1)

# Case 3
ak_tilde <- function(t,k,om,op,lm,lp) {
  return((om*op/(lp-lm))^k/(op*factorial(k-1))*gk(t,k-1,lp))
}
hat_lerch <- function(x,lm,lp){
  return(lerch(x, s = 1, v = 1-lm/lp))
}

c3init <- function(t,om,op,lm,lp) {
  return(op/lm*exp(lm*t))
}
c3odd <- function(t,k,om,op,lm,lp) {
  return(op^2/lp*ak_tilde(t,k,om,op,lm,lp)*hat_lerch(-op*ak_tilde(t,k,om,op,lm,lp),lm,lp))
}
c3even <- function(t,k,om,op,lm,lp) {
  return(op/lp*(lp-lm)*ak_tilde(t,k,om,op,lm,lp)*hat_lerch(-(lp-lm)*ak_tilde(t,k,om,op,lm,lp),lm,lp))
}

cdf3 <- function(t,n,om,op,lm,lp) {
  if(n == 1) {
    return(1.0/(c3init(t,om,op,lm,lp)+1.0))
  }
  else if(n %% 2 != 0) {
    k = (n-1)/2
    return(1.0/(c3odd(t,k,om,op,lm,lp)+1.0))
  }
  else if(n %% 2 == 0) {
    k = n/2
    return(1.0/(c3even(t,k,om,op,lm,lp)+1.0))
  }
}

ggplot() +
  geom_function(fun = cdf3, args = list(n=1,om=0.01,op=0.001,lm=1.0,lp=1.5), aes(color = "1")) +
  geom_function(fun = cdf3, args = list(n=2,om=0.01,op=0.001,lm=1.0,lp=1.5), aes(color = "2")) +
  geom_function(fun = cdf3, args = list(n=3,om=0.01,op=0.001,lm=1.0,lp=1.5), aes(color = "3")) +
  xlim(0,15) + ylim(0,1)

ggplot() +
  geom_function(fun = cdf2, args = list(n=1,om=0.01,op=0.001,l=1.2), aes(color = "0.01")) +
  geom_function(fun = cdf2, args = list(n=1,om=0.005,op=0.005,l=1.2), aes(color = "0.005")) +
  geom_function(fun = cdf2, args = list(n=1,om=0.001,op=0.01,l=1.2), aes(color = "0.001")) +
  xlim(0,15) + ylim(0,1)

###########################################################################
df1_theory <- data.frame(case = c(1,1,2,2,3,3),times = c(0.288,0.702,0.334,0.641,0.441,0.702))
df1_sim <- data.frame(case = c(1,1,2,2,3,3), times = c(0.33,0.69,0.38,0.66,0.49,0.69), errors = c(0.11,0.08,0.12,0.09,0.11,0.08))

plot1 <- ggplot() +
  geom_jitter(data=df1_theory, aes(x=case,y=times,color="thoery"),size=4, shape = 17, height = 0, width = 0.03) + 
  geom_point(data=df1_sim, aes(x=case,y=times,color="simulation"),size=3) +
  geom_errorbar(data=df1_sim, aes(x=case, ymin=times-errors, ymax=times+errors),width=.08,color="black") +
  scale_x_continuous(breaks = c(1,2,3)) +
  scale_color_manual(values=c("black","chartreuse3")) +
  labs(col=NULL) +
  guides(colour = guide_legend(override.aes = list(shape = c(16,17)))) +
  theme(legend.position = "none")
plot1

df2_theory <- data.frame(case = c(1,1,2,2,3,3),times = c(0.265,0.500,0.300,0.459,0.380,0.500))
df2_sim <- data.frame(case = c(1,1,2,2,3,3), times = c(0.27,0.51,0.30,0.50,0.38,0.53), errors = c(0.09,0.06,0.09,0.07,0.08,0.06))

plot2 <- ggplot() +
  geom_jitter(data=df2_theory, aes(x=case,y=times),size=4, shape = 17,color="chartreuse3",height = 0, width = 0.1) + 
  geom_point(data=df2_sim, aes(x=case,y=times),size=3) +
  geom_errorbar(data=df2_sim, aes(x=case, ymin=times-errors, ymax=times+errors),width=.08,color="black") +
  scale_x_continuous(breaks = c(1,2,3)) 
plot2

df3_theory <- data.frame(case = c(1,1,2,2,3,3),times = c(0.462,0.762,0.530,0.750,0.691,0.876))
df3_sim <- data.frame(case = c(1,1,2,2,3,3), times = c(0.46,0.74,0.53,0.72,0.68,0.83), errors = c(0.14,0.11,0.15,0.12,0.16,0.13))

plot3 <- ggplot() +
  geom_jitter(data=df3_theory, aes(x=case,y=times),size=4, shape = 17,color="chartreuse3",height = 0, width = 0.1) + 
  geom_point(data=df3_sim, aes(x=case,y=times),size=3) +
  geom_errorbar(data=df3_sim, aes(x=case, ymin=times-errors, ymax=times+errors),width=.08,color="black") +
  scale_x_continuous(breaks = c(1,2,3)) 
plot3
plot1 + plot2 + plot3

ggsave("./imgs/switching_times/jitter.png",dpi=600)

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
