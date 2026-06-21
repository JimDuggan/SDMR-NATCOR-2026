library(deSolve)
library(tibble)
library(purrr)
library(glue)
library(dplyr)
library(ggplot2)

sirqv_model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{ 
    N           <- S+I+IQ+R+SV
    Attack_Rate <- R/N
    CVF         <- Rollout_Fraction*(Max_Vacc_Fraction-VF)
    
    VR  <-  S*VF
    
    Tx_Coefficent <- R0/Recovery_Delay
    Lambda <- Tx_Coefficent*(I+Tx_Modifier*IQ)/N
    
    IRQ <- Quarantine_Fraction*Lambda*S
    IR  <- (1-Quarantine_Fraction)*Lambda*S
    
    RRQ <- IQ/Recovery_Delay
    
    RR  <- I/Recovery_Delay
    
  
    dS_dt  <- -IR -IRQ -VR
    dI_dt  <-  IR - RR
    dIQ_dt <-  IRQ - RRQ
    dR_dt  <-  RR + RRQ
    dSV_dt <- VR
    dVF_dt <- CVF


    return (list(c(dS_dt,dI_dt,dIQ_dt,dR_dt,dSV_dt,dVF_dt),Lambda=Lambda,N=N,
                 FL_CVF=CVF,FL_VR=VR,FL_IRQ=IRQ,FL_IR=IR,FL_RRQ=RRQ,FL_RR=RR,
                 Rollout_Fraction=Rollout_Fraction,Max_Vacc_Fraction=Max_Vacc_Fraction,
                 Quarantine_Fraction=Quarantine_Fraction,Tx_Modifier=Tx_Modifier,
                 R0=R0,Recovery_Delay=Recovery_Delay))   
  })
}


# It's useful to wrap the call to ode with a function
run_sirqv_model<- function(start=0, 
                           finish=40,
                           step=1/8,
                           Rollout_Fraction=0.1,
                           Max_Vacc_Fraction=0.05,
                           Quarantine_Fraction=0.1,
                           Tx_Modifier=0.5,
                           R0=2,
                           Recovery_Delay=2,
                           inits=c(S=99999,I=1,IQ=0,R=0,SV=0,VF=0)){

  simtime <- seq(start, finish, step)
  # initialise vector of stocks
  stocks  <- c(inits["S"],
               inits["I"],
               inits["IQ"],
               inits["R"],
               inits["SV"],
               inits["VF"])
  
  # initialise vector of auxiliaries
  auxs    <- c(Rollout_Fraction=Rollout_Fraction,
               Max_Vacc_Fraction=Max_Vacc_Fraction,
               Quarantine_Fraction=Quarantine_Fraction,
               Tx_Modifier=Tx_Modifier,
               R0=R0,
               Recovery_Delay=Recovery_Delay)
  
  
  sim <-data.frame(ode(y=stocks, 
                       times  = simtime, 
                       func   = sirqv_model, 
                       parms  = auxs, 
                       method = "euler"))
  
  as_tibble(sim)
}

# One single run, default values
sim <- run_sirqv_model()

p1 <- ggplot(sim,aes(x=time,y=Lambda)) + 
        geom_point()+
        geom_line()
