library(deSolve)
library(dplyr)
library(ggplot2)

bdg_model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{ 
    # Implement the 100+STEP(100,2)-STEP(100,3) function
    Customer_Orders <- 100
    if(time >= 2 & time <3) 
      Customer_Orders <- 200
    
    
    CMS                  <- (Desired_Inventory-Inventory)^2
    Payoff               <- sqrt(Model_Score)
    Shipment_Rate        <- min(Inventory, Customer_Orders)
    CEO                  <- (Customer_Orders-Expected_Orders)*EOATF
    Desired_Supply_Line  <- Delivery_Delay * Expected_Orders 
    
    Inventory_Adjustment <- (Desired_Inventory-Inventory)*SATF
    SL_Adjustment        <- (Desired_Supply_Line-Supply_Line)*SLATF
    Indicated_Orders     <- Expected_Orders + 
                            Inventory_Adjustment + 
                            SL_Adjustment
    Order_Rate           <- max (0, Indicated_Orders)
    Acquisition_Rate     <- Supply_Line/Delivery_Delay
    

    dSL_dt               <- Order_Rate - Acquisition_Rate
    dI_dt                <- Acquisition_Rate - Shipment_Rate
    dEO_dt               <- CEO 
    dPayoff_dt           <- CMS 

    return (list(c(dSL_dt,dI_dt,dEO_dt,dPayoff_dt),
                 FL_OR=Order_Rate,FL_AR=Acquisition_Rate,FL_SR=Shipment_Rate,
                 FL_CMS=CMS,FL_CEO=CEO,Payoff=Payoff,
                 Delivery_Delay=Delivery_Delay,SATF=SATF,SLATF=SLATF,
                 EOATF=EOATF,Desired_Inventory=Desired_Inventory,
                 Desired_Supply_Line=Desired_Supply_Line,
                 SL_Adjustment=SL_Adjustment))   
  })
}


# It's useful to wrap the call to ode with a function
run_bdg_model<- function(start=0, 
                          finish=40,
                          step=1/4,
                          Delivery_Delay=3,
                          SATF=1/4,
                          SLATF=1/4,
                          EOATF=0.50,
                          Desired_Inventory=400,
                          inits=c(Supply_Line=300,Inventory=400,Expected_Orders=100)){

  simtime <- seq(start, finish, step)
  # initialise vector of stocks
  stocks  <- c(inits["Supply_Line"],
               inits["Inventory"],
               inits["Expected_Orders"],
               Model_Score=0)
  
  # initialise vector of auxiliaries
  auxs    <- c(Delivery_Delay=Delivery_Delay,
               SATF=SATF,
               SLATF=SLATF,
               EOATF=EOATF,
               Desired_Inventory=Desired_Inventory)
  
  
  sim <-data.frame(deSolve::ode(y=stocks, 
                                times  = simtime, 
                                func   = bdg_model, 
                                parms  = auxs, 
                                method = "euler"))
  
  dplyr::as_tibble(sim)
}

# One single run, default values
sim <- run_bdg_model()

p1 <- ggplot(sim,aes(x=time,y=Inventory)) + 
        geom_point()+
        geom_line()
