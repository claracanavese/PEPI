require(dplyr)
require(ggplot2)

t = seq(7, 8, 0.01)
z = seq(1000, 150000, 100)

om = 0.005
op = 0.015

lstar = function(lm,lp,om,op){ max(lm, lp) + (om*op)/(lm-lp)}

# 1
am = 1.5
ap = 1.2

bm = 0
bp = 0

lm = am - bm
lp = ap - bp

cm1 = function(am,lm,lp,om,op,t){(lm/am)*exp(-1 * lstar(lm,lp,om,op) * t)}
cp1 = function(am,lm,lp,om,op,t){cm(am,lm,lp,om,op,t) * (lm-lp) / op}

cm2 = function(am,lm,om,op,t){(lm/(am*cosh(sqrt(om*op)*t)))*exp(-1 * lm * t)}
cp2 = function(am,lm,om,op,t){(lm/(am*sinh(sqrt(om*op)*t)))*sqrt(om/op)*exp(-1 * lm * t)}

dp = function(am,lm,lp,om,op,t){(am/lp)*(op*pi)/(ap*sin(pi*lm/lp))*(ap/lp)^(lm/lp)*exp((lp + (om*op*lm)/((lp - lm)*lp))*t)/gamma(1.0-lm/lp)}
dm = function(am,lm,lp,om,op,t){dp(am,lm,lp,om,op,t)*(om/(lp-lm))^(lm/lp)}

pmz = function(am,lm,lp,om,op,t, z){
  if (lm>lp) {
    cm1(am,lm,lp,om,op,t) * exp(-cm1(am,lm,lp,om,op,t) * z)
  }
  else if (lm==lp) {
    cm2(am,lm,om,op,t) * exp(-cm2(am,lm,om,op,t) * z)
  }
  else {
    dm(am,lm,lp,om,op,t)*z^(-lm/lp-1)
  }
}
ppz = function(am,lm,lp,om,op,t, z){cp1(am,lm,lp,om,op,t) * exp(-cp1(am,lm,lp,om,op,t) * z)}

pmz3 = function(am,lm,lp,om,op,t, z){dm(am,lm,lp,om,op,t)*z^(-lm/lp-1)}
ppz3 =function(am,lm,lp,om,op,t, z){dp(am,lm,lp,om,op,t)*z^(-lm/lp-1)}

pm_v = Vectorize(pmz, 'z')
pp_v = Vectorize(ppz, 'z')

density = expand.grid(t, z) %>% 
  as_tibble %>% 
  rename(t = Var1, z = Var2) %>% 
  mutate(
    pmz = pmz(t, z),
    #ppz = ppz(t, z)
  ) %>% 
  group_by(t) %>% 
  mutate(
    pmz = pmz/sum(pmz),
    #ppz = ppz/sum(ppz)
  )

pm = ggplot(density) +
  geom_tile(aes(x=t, y=z, fill = pmz)) +
  scale_fill_distiller(palette = 'Spectral')

pp = ggplot(density) +
  geom_tile(aes(x=t, y=z, fill = ppz)) +
  scale_fill_distiller(palette = 'Spectral')

pm / pp

ggplot() +
  geom_function(aes(color = "1"), fun = dexp, args = list(rate=cm1(1.5,1.5,1.0,0.01,0.01,1))) +
  geom_function(aes(color = "2"), fun = dexp, args = list(rate=cm1(1.5,1.5,1.0,0.01,0.01,2))) +
  geom_function(aes(color = "3"), fun = dexp, args = list(rate=cm1(1.5,1.5,1.0,0.01,0.01,3))) +
  geom_function(aes(color = "4"), fun = dexp, args = list(rate=cm1(1.5,1.5,1.0,0.01,0.01,4))) +
  geom_function(aes(color = "5"), fun = dexp, args = list(rate=cm1(1.5,1.5,1.0,0.01,0.01,5))) +
  #geom_function(aes(color = "6"),fun = dexp, args = list(rate=cm1(1.5,1.5,1.0,0.01,0.01,6)))+
  #stat_function(fun = dexp, args = list(rate=cp(0)), color = "blue") +
  xlim(0,100000)

ggplot() +
  #geom_function(aes(colour = "1"), fun = powerlaw, args = list(coeff=dm(1.0,1.0,1.5,0.01,0.01,1), exponent = -1-1.0/1.5)) +
  geom_function(aes(colour = "1"), fun = pmz, args = list(am=1.5,lm=1.5,lp=1,om=0.01,op=0.01,t=1)) +
  geom_function(aes(colour = "2"), fun = pmz, args = list(am=1.5,lm=1.5,lp=1.5,om=0.01,op=0.01,t=1)) +
  xlim(0,10)

powerlaw <- function(z,coeff,exponent) {
  return(coeff*z^exponent)
}

ggplot() +
  stat_function(fun = cm, color = "blue") + xlim(1,8) +
  stat_function(fun = cp, color = "red")

meanm <- function(t){1./cm(t)}
meanp <- function(t){1./cp(t)}

ggplot() +
  stat_function(fun = meanm, color = "blue") +
  stat_function(fun = meanp, color = "red") +
  xlim(1,8)

alpham = c(1.5,1.0)
alphap = c(1.5,1.0)
omegam = c(0.01,0.001)
omegap = c(0.01,0.001)

marginalsp <- ggplot()
i = 1
for (am in alpham) {
  for (ap in alphap) {
    for (om in omegam) {
      for (op in omegap) {
        lm = am
        lp = ap
        colour = paste("",i,sep="")
        marginalsp = marginalsp + geom_function(aes(colour = colour),fun = pmz, args = list(am,lm,lp,om,op,1))
        i = i+1
      }
    }
  }
}
marginalsp + xlim(0,1000)
paste("",i,sep="")

ggplot() +
  geom_function(aes(colour = "1"),fun = pmz, args = list(alpham[1],alpham[1],alphap[1],omegam[1],omegap[1],1)) +
  geom_function(aes(colour = "2"),fun = pmz, args = list(alpham[1],alpham[1],alphap[1],omegam[1],omegap[2],1)) +
  geom_function(aes(colour = "3"),fun = pmz, args = list(alpham[1],alpham[1],alphap[1],omegam[2],omegap[1],1)) +
  geom_function(aes(colour = "4"),fun = pmz, args = list(alpham[1],alpham[1],alphap[2],omegam[1],omegap[1],1)) +
  geom_function(aes(colour = "5"),fun = pmz, args = list(alpham[1],alpham[1],alphap[2],omegam[2],omegap[1],1)) +
  geom_function(aes(colour = "6"),fun = pmz, args = list(alpham[1],alpham[1],alphap[1],omegam[2],omegap[2],1)) +
  geom_function(aes(colour = "7"),fun = pmz, args = list(alpham[1],alpham[1],alphap[2],omegam[2],omegap[2],1)) +
  geom_function(aes(colour = "8"),fun = pmz, args = list(alpham[2],alpham[2],alphap[1],omegam[1],omegap[1],1)) +
  geom_function(aes(colour = "9"),fun = pmz, args = list(alpham[2],alpham[2],alphap[1],omegam[1],omegap[2],1)) +
  geom_function(aes(colour = "10"),fun = pmz, args = list(alpham[2],alpham[2],alphap[1],omegam[2],omegap[1],1)) +
  geom_function(aes(colour = "11"),fun = pmz, args = list(alpham[2],alpham[2],alphap[2],omegam[1],omegap[1],1)) +
  geom_function(aes(colour = "11"),fun = pmz, args = list(alpham[2],alpham[2],alphap[2],omegam[2],omegap[1],1)) +
  geom_function(aes(colour = "11"),fun = pmz, args = list(alpham[2],alpham[2],alphap[2],omegam[1],omegap[2],1)) +
  geom_function(aes(colour = "11"),fun = pmz, args = list(alpham[2],alpham[2],alphap[1],omegam[2],omegap[2],1)) +
  geom_function(aes(colour = "11"),fun = pmz, args = list(alpham[2],alpham[2],alphap[2],omegam[2],omegap[2],1)) +
  xlim(1,1000)

# lm > lp
ggplot() +
  geom_function(aes(colour = "1"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[2], om = omegam[1], op = omegap[1], t = 1)) +
  geom_function(aes(colour = "2"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[2], om = omegam[1], op = omegap[2], t = 1)) +
  geom_function(aes(colour = "3"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[2], om = omegam[2], op = omegap[1], t = 1)) +
  geom_function(aes(colour = "4"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[2], om = omegam[2], op = omegap[2], t = 1)) +
  xlim(0,10)

# lm = lp
ggplot() +
  geom_function(aes(colour = "5"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[1], om = omegam[1], op = omegap[1], t = 1)) +
  geom_function(aes(colour = "6"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[1], om = omegam[1], op = omegap[2], t = 1)) +
  geom_function(aes(colour = "7"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[1], om = omegam[2], op = omegap[1], t = 1)) +
  geom_function(aes(colour = "8"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[1], om = omegam[2], op = omegap[2], t = 1)) +
  xlim(0,10)

ggplot() +
  geom_function(aes(colour = "1"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[1], om = omegam[1], op = omegap[1], t = 1)) +
  geom_function(aes(colour = "2"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[1], om = omegam[1], op = omegap[2], t = 1)) +
  geom_function(aes(colour = "3"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[1], om = omegam[2], op = omegap[1], t = 1)) +
  geom_function(aes(colour = "6"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[1], om = omegam[2], op = omegap[2], t = 1)) +
  xlim(0,10)

# lm < lp
ggplot() +
  geom_function(aes(colour = "1"),fun = pmz, args = list(am = alpham[2], lm = alpham[2], lp = alphap[1], om = omegam[1], op = omegap[1], t = 1)) +
  geom_function(aes(colour = "2"),fun = pmz, args = list(am = alpham[2], lm = alpham[2], lp = alphap[1], om = omegam[1], op = omegap[2], t = 1)) +
  geom_function(aes(colour = "3"),fun = pmz, args = list(am = alpham[2], lm = alpham[2], lp = alphap[1], om = omegam[2], op = omegap[1], t = 1)) +
  geom_function(aes(colour = "6"),fun = pmz, args = list(am = alpham[2], lm = alpham[2], lp = alphap[1], om = omegam[2], op = omegap[2], t = 1)) +
  xlim(0,1000)

# om = op
ggplot() +
  geom_function(aes(colour = "1"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[2], om = omegam[1], op = omegap[1], t = 1)) +
  geom_function(aes(colour = "2"),fun = pmz, args = list(am = alpham[1], lm = alpham[1], lp = alphap[1], om = omegam[1], op = omegap[1], t = 1)) +
  geom_function(aes(colour = "3"),fun = pmz, args = list(am = alpham[2], lm = alpham[2], lp = alphap[2], om = omegam[1], op = omegap[1], t = 1)) +
  geom_function(aes(colour = "4"),fun = pmz, args = list(am = alpham[2], lm = alpham[2], lp = alphap[1], om = omegam[1], op = omegap[1], t = 1)) +
  xlim(0,10)
  