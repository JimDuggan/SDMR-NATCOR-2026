library(tidyverse)
library(tidymodels)
library(vip)
library(ggplot2)
library(glue)
library(ranger)
library(DALEXtra)


# Read the simulation data and generate subset
sim_data <- readRDS("R/TM/ml_input_tm.rds") %>%
  select(-c(Run))

sim_split <- initial_split(sim_data)
sim_train <- training(sim_split)
sim_test  <- testing(sim_split)

tune_info <- readRDS("R/TM/tuned_rf_tm.rds")


rf_fit <- tune_info$FIT
rf_wkfl <- tune_info$WORK

collect_metrics(rf_fit)
show_best(rf_fit,metric="rmse")


# Fit according the the best hyperparams
rf_tuned <- rf_wkfl %>%
  finalize_workflow(select_best(rf_fit,metric="rmse")) %>%
  fit(data=sim_train)


explainer_rf <- explain(rf_tuned,
                        data=sim_test[,-1],
                        y=sim_test$Payoff,
                        label="random forest")

pdp_rf <- model_profile(explainer_rf, N=1000)

plot(pdp_rf)

plot(pdp_rf,geom="profiles")
