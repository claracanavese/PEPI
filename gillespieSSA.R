library("GillespieSSA2")

# parameters
params <- c(am=1.2, bm=0.0, ap=1.2, bp=0.0, om=0.01, op=0.001)
final_time <- 15
sim_name <- "Switching Process"
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

# explicit tau-leap
set.seed(1)

# simulation
out <- ssa(
  initial_state = initial_state,
  reactions = reactions,
  params = params,
  final_time = final_time,
  method = ssa_etl(tau = 0.1),
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

final_time <- rep(15,nrow(parameters))
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
    method = ssa_etl(tau = 0.1),
    sim_name = sim_name,
    log_firings = TRUE
  )
  # create dataframe to save
  state <- as.data.frame(cbind(out$time,out$state,out$firings))
  colnames(state)[1] <- "time"
  saveRDS(state, file = paste0("./R/simulations/simulation_",i,".rds"))
}

easypar::run( FUN = simulation,
              PARAMS = lapply(1:nrow(all_params), list),
              parallel = TRUE,
              filter_errors = FALSE,
              export = ls(globalenv())
)

