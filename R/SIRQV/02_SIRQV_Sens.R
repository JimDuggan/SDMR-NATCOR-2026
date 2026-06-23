library(purrr)
library(mirai)
library(dplyr)
library(ggplot2)
library(glue)


source("R/SIRQV/01_SIRQV_Model.R")

# Setup sensitivity sweep
NSIMS <- 10000
sim_sirqv_args <- list(
  Rollout_Fraction=runif(NSIMS),
  Quarantine_Fraction=runif(NSIMS),
  Tx_Modifier=runif(NSIMS),
  R0=runif(NSIMS,min=1.5,max=4)
)

cat("Start sensitivity...",format(Sys.time(), "%a %b %d %X %Y"),"\n")
daemons(4,output = TRUE)

# Run simulations in parallel
sens <- pmap(sim_sirqv_args,in_parallel(~{
  out_sim <- run_sirqv_model(Rollout_Fraction = ..1,
                             Quarantine_Fraction = ..2,
                             Tx_Modifier = ..3,
                             R0 = ..4)
  out_sim
},run_sirqv_model=run_sirqv_model,
sirqv_model=sirqv_model))

daemons(0)

cat(format(Sys.time(), "%a %b %d %X %Y"),"\n")

# Add a column for the run number
sens <- map2(1:NSIMS,sens,\(x,y)mutate(y,Run=x) |> select(Run,everything()))

full_sims <- bind_rows(sens)

p2 <- ggplot(full_sims,aes(x=time,y=Lambda,colour=Run,group=Run))+
  geom_line()+scale_colour_gradientn(colours=rainbow(10))


ml_input_sirqv <- full_sims %>%
  group_by(Run) %>%
  summarise(Attack_Rate=last(Attack_Rate),
            Rollout_Fraction=first(Rollout_Fraction),
            Quarantine_Fraction=first(Quarantine_Fraction),
            R0=first(R0),
            Tx_Modifier=first(Tx_Modifier)) %>%
  ungroup()


# Plot the results
p3 <- ggplot(ml_input_sirqv,aes(x=Rollout_Fraction,y=Quarantine_Fraction,size=Attack_Rate,colour=Attack_Rate))+
  geom_point()+
  scale_color_gradient(low="blue", high="red")+geom_jitter()


saveRDS(ml_input_sirqv,"R/SIRQV/ml_input_sirqv.rds")




