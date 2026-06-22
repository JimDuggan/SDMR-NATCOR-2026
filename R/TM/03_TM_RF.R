library(tidyverse)
library(tidymodels)
library(vip)
library(ggplot2)
library(glue)

# Overall process acknowledgment: https://stackoverflow.com/questions/60368047/tidymodels-ranger-with-cross-validation

# Read the simulation data and generate subset
sim_data <- readRDS("R/TM/ml_input_tm.rds") %>%
  select(-Run)


# Create training and testing datasets
sim_split <- initial_split(sim_data)
sim_train <- training(sim_split)
sim_test  <- testing(sim_split)

# Create the model object
rf_model <- rand_forest(mtry=tune(),
                        trees=tune(),
                        min_n = tune()) %>%
  set_engine("ranger",importance="impurity") %>%
  set_mode("regression")

rf_model %>% translate()

# Set the workflow
rf_wkfl <- workflow() %>%
  add_formula(Payoff ~ .) %>%
  add_model(rf_model)

# Choose hyperparameter grid
rf_grid <- grid_space_filling(mtry(range=c(1,20)),
                              trees(range=c(500,1000)),
                              min_n(range=c(2,10)),
                              size=4)

# Setup crossfold validation method
cv_fold <- vfold_cv(sim_train,5)

# Choose metrics
rf_mets <- metric_set(rmse)

# Tune mode based on hyperparameter samples
rf_fit <- tune_grid(rf_wkfl,
                    resamples = cv_fold,
                    grid=rf_grid,
                    metrics=rf_mets,
                    control=control_grid(verbose = TRUE))

rf_tuned <- rf_wkfl %>%
  finalize_workflow(select_best(rf_fit,metric="rmse")) %>%
  fit(data=sim_train)


predict_tr <- sim_train %>%
  select(Payoff) %>%
  bind_cols(predict(rf_tuned,sim_train)) %>%
  mutate(DS=as_factor("Training"))

predict_ts <- sim_test %>%
  select(Payoff) %>%
  bind_cols(predict(rf_tuned,sim_test)) %>%
  mutate(DS=as_factor("Test"))

# Merge datasets
all_data <- predict_tr %>%
  bind_rows(predict_ts)

# Display actual vs predicted
ggplot(all_data,aes(y=Payoff,x=.pred,colour=DS))+geom_point()+
  geom_abline(slope=1,intercept=0)+
  expand_limits(x = 0, y = 0)+
  xlab("Actual")+ylab("Predicted")


saveRDS(list(FIT=rf_fit,
             WORK=rf_wkfl), "R/TM/tuned_rf_tm.rds")

