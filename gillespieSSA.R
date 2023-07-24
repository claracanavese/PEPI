library("GillespieSSA2")

# parameters
params <- c(am=1.5, bm=0.0, ap=1.2, bp=0.0, om=0.01, op=0.001)
final_time <- 8
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

# exact method, TOO SLOW
# set.seed(1)
# out <- ssa(
#   initial_state = initial_state,
#   reactions = reactions,
#   params = params,
#   final_time = final_time,
#   method = ssa_exact(),
#   sim_name = sim_name, 
#   log_propensity = TRUE,
#   log_firings = TRUE
# )
# plot_ssa(out)

# explicit tau-leap
set.seed(1)
out <- ssa(
  initial_state = initial_state,
  reactions = reactions,
  params = params,
  final_time = final_time,
  method = ssa_etl(tau = 0.1),
  sim_name = sim_name,
  log_firings = TRUE
)
plot_ssa(out)
firings <- as.data.frame(cbind(out$time,out$firings))
colnames(firings)[1] <- "time"

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
