library(purrr)
library(mirai)
library(dplyr)
library(ggplot2)


source("R/TM/01_Temp_Model.R")

# Setup sensitivity sweep
NSIMS <- 10000
sim_args <- list(
  alpha=runif(NSIMS),
  beta=runif(NSIMS)
)

cat("Cores = ",parallel::detectCores()," >> ",format(Sys.time(), "%a %b %d %X %Y"),"\n")
daemons(parallel::detectCores(),output = TRUE)

# Run simulations in parallel
sens <- pmap(sim_args,in_parallel(~{

  out_sim <- run_temp_model(Alpha = ..1,
                            Beta = ..2)
  out_sim
},run_temp_model=run_temp_model,
temp_model=temp_model))

daemons(0)

cat(format(Sys.time(), "%a %b %d %X %Y"),"\n")

# Add a column for the run number
sens <- map2(1:NSIMS,sens,\(x,y)mutate(y,Run=x) |> select(Run,everything()))

full_sims <- bind_rows(sens)

p2 <- ggplot(full_sims,aes(x=time,y=Actual_RT,colour=Run,group=Run))+
  geom_line()+scale_colour_gradientn(colours=rainbow(10))


ml_input_tm  <- full_sims %>%
  group_by(Run) %>%
  summarise(Payoff=last(Payoff),
            alpha=first(Alpha),
            beta=first(Beta))


# Plot the results
p3 <- ggplot(ml_input_tm,aes(x=alpha,y=beta,size=Payoff,colour=Payoff))+
  geom_point()+
  scale_color_gradient(low="blue", high="red")+geom_jitter()

saveRDS(ml_input_tm,"R/TM/ml_input_tm.rds")




