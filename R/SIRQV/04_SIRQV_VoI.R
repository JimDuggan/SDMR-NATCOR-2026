library(tidyverse)
library(tidymodels)
library(vip)
library(ggplot2)
library(glue)
library(ranger)
library(DALEXtra)


# Read the simulation data and generate subset
sim_data <- readRDS("R/SIRQV/ml_input_sirqv.rds") %>%
  select(-c(Run))

sim_split <- initial_split(sim_data)
sim_train <- training(sim_split)
sim_test  <- testing(sim_split)

tune_info <- readRDS("R/SIRQV/tuned_rf_sirqv.rds")


rf_fit <- tune_info$FIT
rf_wkfl <- tune_info$WORK

collect_metrics(rf_fit)
show_best(rf_fit,metric="rmse")


# Fit according the the best hyperparams
rf_tuned <- rf_wkfl %>%
  finalize_workflow(select_best(rf_fit,metric="rmse")) %>%
  fit(data=sim_train)



# ------------------------------------------------------------------------------
explainer_rf <- explain(rf_tuned,
                        data=sim_test[,-1],
                        y=as.numeric(unlist(sim_test$Attack_Rate)),
                        label="random forest")


loss <- loss_root_mean_square(observed = sim_test$Attack_Rate,
                              predicted = pull(predict(rf_tuned, sim_test)))

set.seed(100)


mp <- model_parts(explainer=explainer_rf,
                  loss_function=loss_root_mean_square,
                  B=1)


mp_25 <- model_parts(explainer=explainer_rf,
                     loss_function=loss_root_mean_square,
                     B=25,
                     type="variable_importance")





