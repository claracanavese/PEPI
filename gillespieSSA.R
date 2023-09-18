library("GillespieSSA2")
library("easypar")
library("StanHeaders")
library(dplyr)
library(magrittr)
library(ggplot2)

# parameters
params <- c(am=1.5, bm=0.0, ap=1.0, bp=0.0, om=0.01, op=0.001)
final_time <- 5
sim_name <- "Switching Process"
# initial state
initial_state <- c(zm=1000, zp=100)
# reactions
reactions <- list(
  reaction("am*zm", c(zm = +1), name = "birth_minus"),
  reaction("bm*zm", c(zm = -1), name = "death_minus"),
  reaction("ap*zp", c(zp = +1), name = "birth_plus"),
  reaction("bp*zp", c(zp = -1), name = "death_plus"),
  reaction("op*zm", c(zp = +1), name = "switch_to_plus"),
  reaction("om*zp", c(zm = +1), name = "switch_to_minus")
)

simulation1 = function(i){
  library("GillespieSSA2")
  params <- c(am=1.0, bm=0.0, ap=1.5, bp=0.0, om=0.005, op=0.005)
  sim_name <- paste0("Switching Process ",i)
  # initial state
  initial_state <- c(zm=1, zp=0)
  # reactions
  reactions <- list(
    reaction("am*zm", c(zm = +1), name = "birth_minus"),
    reaction("bm*zm", c(zm = -1), name = "death_minus"),
    reaction("ap*zp", c(zp = +1), name = "birth_plus"),
    reaction("bp*zp", c(zp = -1), name = "death_plus"),
    reaction("op*zm", c(zp = +1), name = "switch_to_plus"),
    reaction("om*zp", c(zm = +1), name = "switch_to_minus")
  )
  # simulation
  out <- ssa(
    initial_state = initial_state,
    reactions = reactions,
    params = params,
    final_time = 12,
    method = ssa_exact(),
    sim_name = sim_name,
    log_firings = TRUE
  )
  print(paste0("Switching Process ",i))
  # create dataframe to save
  state <- as.data.frame(cbind(out$time,out$state,out$firings))
  colnames(state)[1] <- "time"
  
  pz <<- bind_rows(pz,state[nrow(state),])
  fs = filter(state, switch_to_plus > 0)
  ss = filter(state, switch_to_minus > 0)
  first_switch[i+400] <<- fs[1,]$time
  second_switch[i+400] <<- ss[1,]$time
}

easypar::run( FUN = simulation1,
              PARAMS = lapply(1:2, list),
              parallel = TRUE,
              filter_errors = FALSE,
              export = ls(globalenv())
)
prova <- lapply(1:100, simulation1)

# pz <- data.frame()
# first_switch = list()
# second_switch = list()


# for (i in seq(1,500)) {
#   sim <- readRDS(paste0("./R/simulations_1.0_1.5_0.001_0.01_10t/simulation_",i,".rds")) %>% tibble::as_tibble()
#   pz <- bind_rows(pz,sim[nrow(sim),])
#   fs = filter(sim, switch_to_plus > 0)
#   ss = filter(sim, switch_to_minus > 0)
#   first_switch[i] = fs[1,]$time
#   second_switch[i] = ss[1,]$time
# }

median(unlist(first_switch), na.rm = TRUE)
sd(unlist(first_switch), na.rm = TRUE)
median(unlist(second_switch), na.rm = TRUE)
sd(unlist(second_switch), na.rm = TRUE)

saveRDS(first_switch, file="./R/1.0_1.5_0.01_0.001_14t_fs.RData")
saveRDS(second_switch, file="./R/1.0_1.5_0.01_0.001_14t_ss.RData")
saveRDS(pz,"./R/1.0_1.5_0.01_0.001_14t.rds")

pz <- readRDS("./R/1.0_1.5_0.01_0.001_14t.rds")
first_switch <- readRDS("./R/1.0_1.5_0.01_0.001_14t_fs.RData")
second_switch <- readRDS("./R/1.0_1.5_0.01_0.001_14t_ss.RData") 
  
# explicit tau-leap
set.seed(1)

# simulation
out <- ssa(
  initial_state = initial_state,
  reactions = reactions,
  params = params,
  final_time = final_time,
  method = ssa_exact(),
  sim_name = sim_name,
  log_firings = TRUE
)
# create dataframe to save
state <- as.data.frame(cbind(out$time,out$state,out$firings))
colnames(state)[1] <- "time"
saveRDS(state, file = "./R/simulation_name.rds")

plot_ssa(out)

firings <- as.data.frame(cbind(out$time,out$firings))
colnames(firings)[1] <- "time"
sum(firings$switch_to_plus)
sum(firings$switch_to_minus)
first_plus = filter(firings, firings$switch_to_plus != 0)[1,]$time
first_minus = filter(firings, firings$switch_to_minus != 0)[1,]$time

state <- as.data.frame(cbind(out$time,out$state,out$firings))
colnames(state)[1] <- "time"
saveRDS(state, file = "./R/1.5_1.2_0.01_0.001.rds")

# binomial tau-leap
set.seed(1)
out <- ssa(
  initial_state = initial_state,
  reactions = reactions,
  params = params,
  final_time = final_time,
  method = ssa_btl(mean_firings = 10),
  sim_name = sim_name,
  log_firings = TRUE
)
plot_ssa(out)

firings <- as.data.frame(cbind(out$time,out$firings))
colnames(firings)[1] <- "time"
sum(firings$switch_to_plus)
sum(firings$switch_to_minus)
first_plus = filter(firings,switch_to_plus == 1)[1,]$time
first_minus = filter(firings,switch_to_minus == 1)[1,]$time

saveRDS(out, file = "./simulations/gillespieSSA2/out.Rdata")
out1 <- readRDS("./simulations/gillespieSSA2/out.Rdata")

rm(list = ls())

parameters <- data.frame(am=1.5, bm=0.0, ap=1.0, bp=0.0, om=0.01, op=0.001)

parameters = rbind(parameters, c(am=1.5, bm=0.0, ap=1.0, bp=0.0, om=0.001, op=0.01))
parameters = rbind(parameters, c(am=1.5, bm=0.0, ap=1.0, bp=0.0, om=0.005, op=0.005))
parameters = rbind(parameters, c(am=1.0, bm=0.0, ap=1.5, bp=0.0, om=0.001, op=0.01))
parameters = rbind(parameters, c(am=1.0, bm=0.0, ap=1.5, bp=0.0, om=0.01, op=0.001))
parameters = rbind(parameters, c(am=1.0, bm=0.0, ap=1.5, bp=0.0, om=0.005, op=0.005))
parameters = rbind(parameters, c(am=1.2, bm=0.0, ap=1.2, bp=0.0, om=0.001, op=0.01))
parameters = rbind(parameters, c(am=1.2, bm=0.0, ap=1.2, bp=0.0, om=0.01, op=0.001))
parameters = rbind(parameters, c(am=1.2, bm=0.0, ap=1.2, bp=0.0, om=0.005, op=0.005))

saveRDS(parameters, file = "./R/parameters.rds")

# easypar
rm(all_params)
final_time <- rep(10,nrow(parameters))
parameters$final_time = final_time
all_params = parameters
simulation_number <-rep(1:100,each=nrow(all_params))
all_params = cbind(all_params, simulation_number)

simulation = function(i){
  library("GillespieSSA2")
  params <- c(am=all_params$am[i], bm=all_params$bm[i], ap=all_params$ap[i], bp=all_params$bp[i], om=all_params$om[i], op=all_params$op[i])
  final_time <- all_params$final_time[i]
  sim_name <- paste0("Switching Process",i)
  # initial state
  initial_state <- c(zm=1000, zp=100)
  # reactions
  reactions <- list(
    reaction("am*zm", c(zm = +1), name = "birth_minus"),
    reaction("bm*zm", c(zm = -1), name = "death_minus"),
    reaction("ap*zp", c(zp = +1), name = "birth_plus"),
    reaction("bp*zp", c(zp = -1), name = "death_plus"),
    reaction("op*zm", c(zp = +1), name = "switch_to_plus"),
    reaction("om*zp", c(zm = +1), name = "switch_to_minus")
  )
  # simulation
  out <- ssa(
    initial_state = initial_state,
    reactions = reactions,
    params = params,
    final_time = final_time,
    method = ssa_exact(),
    sim_name = sim_name,
    log_firings = TRUE
  )
  # create dataframe to save
  state <- as.data.frame(cbind(out$time,out$state,out$firings))
  print(paste0("./pz_sim/simulation_",all_params$am[i],"_",all_params$ap[i],"_",all_params$om[i],"_",all_params$op[i],"_",all_params$simulation_number[i],".rds"))
  colnames(state)[1] <- "time"
  saveRDS(state, file = paste0("./pz_sim/simulation_",all_params$am[i],"_",all_params$ap[i],"_",all_params$om[i],"_",all_params$op[i],"_",all_params$simulation_number[i],".rds"))
}


# simulation_pz = function(i){
#   library("GillespieSSA2")
#   params <- c(am=all_params$am[i], bm=all_params$bm[i], ap=all_params$ap[i], bp=all_params$bp[i], om=all_params$om[i], op=all_params$op[i])
#   final_time <- all_params$final_time[i]
#   sim_name <- paste0("Switching Process",i)
#   # initial state
#   initial_state <- c(zm=1, zp=0)
#   # reactions
#   reactions <- list(
#     reaction("am*zm", c(zm = +1), name = "birth_minus"),
#     reaction("bm*zm", c(zm = -1), name = "death_minus"),
#     reaction("ap*zp", c(zp = +1), name = "birth_plus"),
#     reaction("bp*zp", c(zp = -1), name = "death_plus"),
#     reaction("op*zm", c(zp = +1), name = "switch_to_plus"),
#     reaction("om*zp", c(zm = +1), name = "switch_to_minus")
#   )
#   # simulation
#   out <- ssa(
#     initial_state = initial_state,
#     reactions = reactions,
#     params = params,
#     final_time = final_time,
#     method = ssa_etl(tau = 0.2),
#     sim_name = sim_name,
#     log_firings = TRUE
#   )
#   # create dataframe to save
#   state <- as.data.frame(cbind(out$time,out$state,out$firings))
#   colnames(state)[1] <- "time"
#   saveRDS(state, file = paste0("./simulations_for_pz/simulation_",all_params$am[i],"_",all_params$ap[i],"_",all_params$om[i],"_",all_params$op[i],"_",all_params$simulation_number[i],".rds"))
# }

easypar::run( FUN = simulation,
              PARAMS = lapply(1:3, list),
              parallel = TRUE,
              filter_errors = FALSE,
              export = ls(globalenv())
)

sim1 <- readRDS(paste0("./pz_sim/simulation_",1.5,"_",1,"_",0.01,"_",0.001,"_",1,".rds")) %>% tibble::as_tibble()
sim2 <- readRDS(paste0("./pz_sim/simulation_",1.5,"_",1,"_",0.01,"_",0.001,"_",2,".rds")) %>% tibble::as_tibble()
sim3 <- readRDS(paste0("./pz_sim/simulation_",1.5,"_",1,"_",0.01,"_",0.001,"_",3,".rds")) %>% tibble::as_tibble()
sim4 <- readRDS(paste0("./pz_sim/simulation_",1.5,"_",1,"_",0.01,"_",0.001,"_",4,".rds")) %>% tibble::as_tibble()
sim5 <- readRDS(paste0("./pz_sim/simulation_",1.5,"_",1,"_",0.01,"_",0.001,"_",5,".rds")) %>% tibble::as_tibble()
sim6 <- readRDS(paste0("./pz_sim/simulation_",1.5,"_",1,"_",0.01,"_",0.001,"_",6,".rds")) %>% tibble::as_tibble()
sim7 <- readRDS(paste0("./pz_sim/simulation_",1.5,"_",1,"_",0.01,"_",0.001,"_",7,".rds")) %>% tibble::as_tibble()
sim8 <- readRDS(paste0("./pz_sim/simulation_",1.5,"_",1,"_",0.01,"_",0.001,"_",8,".rds")) %>% tibble::as_tibble()
sim9 <- readRDS(paste0("./pz_sim/simulation_",1.5,"_",1,"_",0.01,"_",0.001,"_",9,".rds")) %>% tibble::as_tibble()

pz <- data.frame()
for (i in seq(1,500)) {
  sim <- readRDS(paste0("./simulations_for_pz_10t/simulation_",1.0,"_",1.5,"_",0.005,"_",0.005,"_",i,".rds")) %>% tibble::as_tibble()
  pz <- bind_rows(pz,sim[51,])
}
saveRDS(pz,"./R/1.0_1.5_0.005_10t.rds")
