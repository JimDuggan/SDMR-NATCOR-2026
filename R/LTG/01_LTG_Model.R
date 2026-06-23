library(deSolve)
library(dplyr)
library(ggplot2)


ltg_model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    C <- P/K             # Eq (2)
    dP_dt <- r*P*(1-C)   # Eq (1)
    return (list(c(dP_dt), 
                 r=r, 
                 K=K,
                 C=C,
                 Flow=dP_dt))
  })
}

stocks  <- c(P=100)            # Eq (5)
auxs    <- c(r=0.15,K=100000)  # Eq (3) and Eq (4)

# It's useful to wrap the call to ode with a function
run_ltg_model<- function(start=0, 
                        finish=100,
                          step=1/4,
                          r=0.15,
                          K=100000,
                          inits=c(P=100)){

  simtime <- seq(start, finish, step)
  # initialise vector of stocks
  stocks  <- c(inits["P"])
  
  # initialise vector of auxiliaries
  auxs    <- c(r=r,
               K=K)
  

  sim <-data.frame(deSolve::ode(y=stocks, 
                                times  = simtime, 
                                func   = ltg_model, 
                                parms  = auxs, 
                                method = "euler"))
  
  dplyr::as_tibble(sim)
}

# One single run, default values
sim <- run_ltg_model()

ggplot(sim,aes(x=time,y=P)) + 
  geom_line()+
  geom_line(aes(y=K), colour="red")+
  labs(title = "Simulation Model Output",
       subtitle = "Limits to growth model",
       x = "Time",
       y = "Population")

