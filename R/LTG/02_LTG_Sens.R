library(purrr)
library(mirai)
library(dplyr)
library(ggplot2)


source("R/LTG/01_LTG_Model.R")

# Setup sensitivity sweep
NSIMS <- 1000
sim_args <- list(
  r=runif(NSIMS,min = 0, max = 0.25)
)

cat("Sensitivity Start >> ",format(Sys.time(), "%a %b %d %X %Y"),"\n")
daemons(4,output = TRUE)

# Run simulations in parallel
sens <- pmap(sim_args,in_parallel(~{

  out_sim <- run_ltg_model(r = ..1)
  out_sim
},run_ltg_model=run_ltg_model,
ltg_model=ltg_model))

daemons(0)

cat(format(Sys.time(), "%a %b %d %X %Y"),"\n")

# Add a column for the run number
sens <- map2(1:NSIMS,sens,\(x,y)mutate(y,Run=x) |> select(Run,everything()))

full_sims <- bind_rows(sens)

p1 <- ggplot(full_sims,aes(x=time,y=Flow,colour=Run,group=Run))+
  geom_line()+scale_colour_gradientn(colours=rainbow(10))

p2 <- ggplot(full_sims,aes(x=time,y=P,colour=Run,group=Run))+
  geom_line()+scale_colour_gradientn(colours=rainbow(10))


sim_agg <- full_sims %>%
            group_by(time) %>%
            summarise(MedP=median(P),
                      Q75=quantile(P,0.75),
                      Q25=quantile(P,0.25))

ggplot(sim_agg,aes(x=time,y=MedP))+
  geom_ribbon(aes(ymin=Q25,ymax=Q75),fill="steelblue2")+
  geom_line(colour="firebrick")





