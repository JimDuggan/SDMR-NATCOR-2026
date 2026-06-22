library(purrr)
library(mirai)
library(dplyr)
library(ggplot2)


source("R/BDG/01_BDG_Model.R")

# Setup sensitivity sweep
NSIMS <- 10000
sim_bdg_args <- list(
  Delivery_Delay=runif(NSIMS,min = 2, max = 6),
  SATF=runif(NSIMS),
  SLATF=runif(NSIMS),
  EOATF=runif(NSIMS)
)

cat("Cores = ",parallel::detectCores()," >> ",format(Sys.time(), "%a %b %d %X %Y"),"\n")
daemons(parallel::detectCores(),output = TRUE)

# Run simulations in parallel
sens <- pmap(sim_bdg_args,in_parallel(~{

  out_sim <- run_bdg_model(Delivery_Delay = ..1,
                           SATF = ..2,
                           SLATF = ..3,
                           EOATF = ..4)
  out_sim
},run_bdg_model=run_bdg_model,
bdg_model=bdg_model))

daemons(0)

cat(format(Sys.time(), "%a %b %d %X %Y"),"\n")

# Add a column for the run number
sens <- map2(1:NSIMS,sens,\(x,y)mutate(y,Run=x) |> select(Run,everything()))

full_sims <- bind_rows(sens)

p2 <- ggplot(full_sims,aes(x=time,y=Inventory,colour=Run,group=Run))+
  geom_line()+scale_colour_gradientn(colours=rainbow(10))


ml_input_bdg <- full_sims %>%
  group_by(Run) %>%
  summarise(Payoff=last(Payoff),
            SATF=first(SATF),
            SLATF=first(SLATF),
            Delivery_Delay=first(Delivery_Delay),
            EOATF=first(EOATF))


# Plot the results
p3 <- ggplot(ml_input_bdg,aes(x=SLATF,y=SATF,size=Payoff,colour=Payoff))+
  geom_point()+
  scale_color_gradient(low="blue", high="red")+geom_jitter()

saveRDS(ml_input_bdg,"R/BDG/ml_input_bdg.rds")




