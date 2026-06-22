library(deSolve)
library(tibble)
library(purrr)
library(glue)
library(dplyr)
library(ggplot2)

temp_model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{ 
    CMS <- (Desired_RT-Actual_RT)^2
    Change_Actual <- (Desired_RT-Perceived_RT)*Alpha
    Change_Perceived <- (Actual_RT-Perceived_RT)*Beta
    Payoff <- sqrt(Model_Score)
    
    dA_dt <- Change_Actual
    dP_dt <- Change_Perceived
    dPayoff_dt <- CMS 

    return (list(c(dA_dt,dP_dt,dPayoff_dt),
                 FL_CA=Change_Actual,FL_CP=Change_Perceived,Payoff=Payoff,
                 Alpha=Alpha,Beta=Beta,Desired_RT=Desired_RT))   
  })
}


# It's useful to wrap the call to ode with a function
run_temp_model<- function(start=0, 
                          finish=100,
                          step=1/16,
                          Desired_RT=20,
                          Alpha=0.1,
                          Beta=0.1,
                          inits=c(Actual_RT=10,Perceived_RT=-10)){

  simtime <- seq(start, finish, step)
  # initialise vector of stocks
  stocks  <- c(inits["Actual_RT"],
               inits["Perceived_RT"],
               Model_Score=0)
  
  # initialise vector of auxiliaries
  auxs    <- c(Desired_RT=Desired_RT,
               Alpha=Alpha,
               Beta=Beta)
  
  
  sim <-data.frame(deSolve::ode(y=stocks, 
                                times  = simtime, 
                                func   = temp_model, 
                                parms  = auxs, 
                                method = "euler"))
  
  dplyr::as_tibble(sim)
}

# One single run, default values
sim <- run_temp_model()

p1 <- ggplot(sim,aes(x=time,y=Actual_RT)) + 
        geom_point()+
        geom_line()
