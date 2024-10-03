library(GGally)
library(corrplot)
library(patchwork)
library(dplyr)

library(tidyverse)
library(tidymodels)
library(vroom)


## Reading in the Data
traindata <- vroom("train.csv")
testdata <- vroom("test.csv")

traindata <- traindata %>%
  select(-registered, -casual)
traindata$count <- log(traindata$count)

## Define a recipe
bike_recipe <- recipe(count~., data = traindata) %>%
  step_mutate(weather =ifelse(weather==4,3, weather)) %>% # recodes weather 4 to 3
  step_mutate(weather = factor(weather, levels = c(1,2,3))) %>% # makes weather a factor
  step_mutate(season = factor(season, levels = c(1,2,3,4))) %>% # makes season a factor
  step_date(datetime, features = "dow") %>% # gets day of week
  step_time(datetime, features ="hour") %>% # gets hour
  step_mutate(datetime_hour, factor(datetime_hour, levels = c(0:23))) # sets hour to factor

## Define a Model
lin_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

## Combine into a Workflow and fit
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data=traindata)

## Run all the steps on test data and change back to normal data
lin_preds_log <- predict(bike_workflow, new_data = testdata)
lin_preds <- exp(lin_preds_log)

## Format the Predictions for Submission to Kaggle
kaggle_submission <-  lin_preds %>%
  bind_cols(., testdata) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out file
vroom_write(x=kaggle_submission, file="./LinearPreds10.csv", delim=",")



###################################### Penalized Regression
library(tidyverse)
library(tidymodels)
library(vroom)
library(tune)

## Reading in the Data
traindata <- vroom("train.csv")
testdata <- vroom("test.csv")

traindata <- traindata %>%
  select(-registered, -casual)
traindata$count <- log(traindata$count)

## Create a recipe
bike_recipe2 <- recipe(count~., data = traindata) %>%
  step_mutate(weather =ifelse(weather==4,3, weather)) %>% # recodes weather 4 to 3
  step_mutate(weather = factor(weather, levels = c(1,2,3))) %>% # makes weather a factor
  step_mutate(season = factor(season, levels = c(1,2,3,4))) %>% # makes season a factor
  step_date(datetime, features = "dow") %>% # gets day of week
  step_time(datetime, features ="hour") %>% # gets hour
  step_mutate(datetime_hour = factor(datetime_hour, levels = c(0:23))) %>% # sets hour to factor
  #step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>% # make dummy variables
  step_normalize(all_numeric_predictors()) %>% # mean = 0, sd = 1
  step_interact(~ all_predictors() * all_predictors()) # interaction between all predictors
  
## Random Forests
rand_for <- rand_forest(mtry = tune(),
                          min_n = tune(),
                          trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

## Regression Tree
reg_tree <- decision_tree(tree_depth = tune(),
                          cost_complexity = tune(),
                          min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

## Penalized regression model
preg_model <- linear_reg(penalty=tune(), mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

## Workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe2) %>%
  add_model(rand_for) 

## Grid of values to tune over (1. penalized 2. Regression Tree 3. rand forest)
grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 2)

grid_of_tuning_params_tree <- grid_regular(tree_depth(),
                                      cost_complexity(),
                                      min_n(),
                                      levels = 5)

grid_of_tuning_params_randfor <- grid_regular(mtry(range = c(1,100)),
                                              min_n(),
                                              levels = 5)

## Split data for CV
folds <- vfold_cv(traindata, v = 5, repeats = 1)

## Run the CV
CV_results <- preg_wf %>%
    tune_grid(
      resamples = folds,
      grid = grid_of_tuning_params_randfor,
      metrics = metric_set(rmse, mae, rsq)
    )


## Find Best Tuning Parameters
bestTune <- CV_results %>%
  select_best(metric = "rmse")

## Finalize the Workflow & fit it
final_wf <- preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=traindata)

## predict
preds <- final_wf %>%
  predict(new_data = testdata)

preg_preds <- exp(preds)

## Format the Predictions for Submission to Kaggle
kaggle_submission <-  preg_preds %>%
  bind_cols(., testdata) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out file
vroom_write(x=kaggle_submission, file="./randfor2.csv", delim=",")





#### EDA

# Correlation Plot
corr <- corrplot(cor(traindata[sapply(traindata, is.numeric)]), type = "upper")

# Humidity vs Count scatterplot with trendline
humid <- ggplot(traindata, mapping=aes(x=humidity, y = count)) +
  geom_point() +
  geom_smooth(se=F)

# Temperature vs Count scatterplot with trendline
temp <- ggplot(traindata, mapping=aes(x=temp, y = count)) +
  geom_point() +
  geom_smooth(se=F)

# Weather bar graph
weather <- ggplot(traindata, mapping = aes(x = weather)) +
  geom_bar()
# only 1 day where weather == 4
sum(data$weather == 4)

# Wind vs Count bar graph
wind <- ggplot(traindata, mapping = aes(x = windspeed, y = count)) +
  geom_point()

# Season bar graph
season <- ggplot(traindata, mapping=aes(x=season)) +
  geom_bar()

# Holiday bar graph
holiday <- ggplot(traindata, mapping = aes(x = holiday)) +
  geom_bar()

# Making the 2x2 plot
(holiday + weather) / (temp + humid)


colnames(traindata)
traindata$season <- as.factor(traindata$season)
traindata$holiday <- as.factor(traindata$holiday)
traindata$weather <- as.factor(traindata$weather)

testdata$season <- as.factor(testdata$season)
testdata$holiday <- as.factor(testdata$holiday)
testdata$weather <- as.factor(testdata$weather)



#### Linear Regression Model

my_linear_model <- linear_reg() %>% #Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") %>% # Regression just means quantitative response
  fit(formula=log(count)~datetime+season+holiday+workingday+weather+
        temp+humidity+windspeed, data=traindata)


## Generate Predictions Using Linear Model
bike_predictions <- predict(my_linear_model,
                            new_data=testdata) # Use fit to predict
bike_predictions <- exp(bike_predictions) ## Look at the output

min(bike_predictions)
## Format the Predictions for Submission to Kaggle
kaggle_submission <- bike_predictions %>%
  bind_cols(., testdata) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")



library(poissonreg)

my_pois_model <- poisson_reg() %>% #Type of model3
  set_engine("glm") %>% # GLM = generalized linear model4
  set_mode("regression") %>%
fit(formula=count~datetime+season+holiday+workingday+weather+
      temp+humidity+windspeed, data=traindata)

## Generate Predictions Using Linear Model8
bike_predictions <- predict(my_pois_model,
                            new_data=testdata) # Use fit to predict10
bike_predictions ## Look at the output


## Format the Predictions for Submission to Kaggle
pois_kaggle_submission <- bike_predictions %>%
  bind_cols(., testdata) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction va
  rename(count=.pred) %>% #rename pred to count (for submission to
  mutate(datetime=as.character(format(datetime))) #needed for right

## Write out the file8
vroom_write(x=pois_kaggle_submission, file="./PoissonPreds.csv", delim=",")




############################## Stacking #########################
library(tidyverse)
library(tidymodels)
library(vroom)
library(tune)
library(stacks)


## Reading in the Data
traindata <- vroom("train.csv")
testdata <- vroom("test.csv")

traindata <- traindata %>%
  select(-registered, -casual)
traindata$count <- log(traindata$count)

## Split data for CV
folds <- vfold_cv(traindata, v = 5, repeats = 1)

## Create a control grid
untunedModel <- control_stack_grid()
tunedModel <- control_stack_resamples()

## Create a recipe
bike_recipe <- recipe(count~., data = traindata) %>%
  step_mutate(weather =ifelse(weather==4,3, weather)) %>% # recodes weather 4 to 3
  step_mutate(weather = factor(weather, levels = c(1,2,3))) %>% # makes weather a factor
  step_mutate(season = factor(season, levels = c(1,2,3,4))) %>% # makes season a factor
  step_date(datetime, features = "dow") %>% # gets day of week
  step_time(datetime, features ="hour") %>% # gets hour
  step_mutate(datetime_hour = factor(datetime_hour, levels = c(0:23))) %>% # sets hour to factor
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>% # make dummy variables
  step_normalize(all_numeric_predictors()) %>% # mean = 0, sd = 1
  step_interact(~ all_predictors() * all_predictors()) # interaction between all predictors

## Penalized regression model
preg_model <- linear_reg(penalty=tune(), mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

## Set workflow - Penalized Reg model
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) 

## Grid of Values to tune over
grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 10)

## Run the CV for Penalized Regression
CV_results_preg <- preg_wf %>%
  tune_grid(
    resamples = folds,
    grid = grid_of_tuning_params,
    metrics = metric_set(rmse, mae, rsq),
    control = untunedModel
  )

## Linear Reg Model
lin_reg <- linear_reg() %>%
  set_engine('lm') %>%
  set_mode("regression")

lin_reg_wf <- workflow() %>%
  add_model(lin_reg) %>%
  add_recipe(bike_recipe)

lin_reg_model <- fit_resamples(
  lin_reg_wf,
  resamples = folds,
  metrics = metric_set(rmse),
  control = tunedModel
)



## Random forests
grid_of_tuning_params_randfor <- grid_regular(mtry(range = c(1,75)),
                                              min_n(),
                                              levels = 8)

rand_for <- rand_forest(mtry = tune(),
                        min_n = tune(),
                        trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rand_for_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(rand_for)

CV_results_randfor <- rand_for_wf %>%
  tune_grid(
    resamples = folds,
    grid = grid_of_tuning_params_randfor,
    metrics = metric_set(rmse, mae, rsq),
    control = untunedModel
  )

## Specify models to use
my_stack <- stacks() %>%
  add_candidates(CV_results_randfor) %>%
  add_candidates(CV_results_preg) %>%
  add_candidates(lin_reg_model)

## Fit stacked model
stack_mod <- my_stack %>%
  blend_predictions() %>%
  fit_members()

## Use stacked data to get a prediction
stack_mod_preds <- predict(stack_mod, new_data = testdata)

stack_preds <- exp(stack_mod_preds)

## Format the Predictions for Submission to Kaggle
kaggle_submission <-  stack_preds %>%
  bind_cols(., testdata) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out file
vroom_write(x=kaggle_submission, file="./stacked1.csv", delim=",")






########################## Trying my own ###################################
library(tidyverse)
library(tidymodels)
library(vroom)
library(tune)
library(stacks)
library(dbarts)


## Reading in the Data
traindata <- vroom("train.csv")
testdata <- vroom("test.csv")

# traindata <- traindata %>%
#   select(-registered, -casual)
# traindata$count <- log(traindata$count)

traindata_reg <- traindata %>%
  select(-casual, -count)
traindata_reg$registered <- log(traindata_reg$registered +0.001)
#traindata_reg$registered[is.infinite(traindata_reg$registered)] <- 0


traindata_cas <- traindata %>%
  select(-registered, -count)
traindata_cas$casual <- log(traindata_cas$casual +0.001)
#traindata_cas$casual[is.infinite(traindata_cas$casual)] <- 0

## Create a recipe_reg
bike_recipe_reg <- recipe(registered~., data = traindata_reg) %>%
  step_mutate(weather =ifelse(weather==4,3, weather)) %>% # recodes weather 4 to 3
  step_mutate(weather = factor(weather, levels = c(1,2,3))) %>% # makes weather a factor
  step_mutate(season = factor(season, levels = c(1,2,3,4))) %>% # makes season a factor
  step_date(datetime, features = "dow") %>% # gets day of week
  step_time(datetime, features ="hour") %>% # gets hour
  step_date(datetime, features = "year") %>%
  step_mutate(datetime_year = factor(datetime_year)) %>%
  step_mutate(datetime_hour = factor(datetime_hour)) %>% # sets hour to factor
  step_interact(~ datetime_hour:workingday) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>% # make dummy variables
  step_normalize(all_numeric_predictors()) # mean = 0, sd = 1
  
  #step_interact(~ all_predictors() * all_predictors()) # interaction between all predictors

## Create a recipe_reg
bike_recipe_cas <- recipe(casual~., data = traindata_cas) %>%
  step_mutate(weather =ifelse(weather==4,3, weather)) %>% # recodes weather 4 to 3
  step_mutate(weather = factor(weather, levels = c(1,2,3))) %>% # makes weather a factor
  step_mutate(season = factor(season, levels = c(1,2,3,4))) %>% # makes season a factor
  step_date(datetime, features = "dow") %>% # gets day of week
  step_time(datetime, features ="hour") %>% # gets hour
  step_date(datetime, features = "year") %>%
  step_mutate(datetime_year = factor(datetime_year)) %>%
  step_mutate(datetime_hour = factor(datetime_hour)) %>% # sets hour to factor
  step_interact(~ datetime_hour:workingday) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>% # make dummy variables
  step_normalize(all_numeric_predictors()) # mean = 0, sd = 1




## Bart Model
bartmodel <- parsnip::bart() %>%
  set_engine("dbarts") %>%
  set_mode("regression") %>%
  set_args(
    trees = 1000
  )

## Workflow_reg
bart_wf <- workflow() %>%
  add_model(bartmodel) %>%
  add_recipe(bike_recipe_reg)

## Fit bart_reg
fitted_bart <- bart_wf %>%
  fit(data = traindata_reg)

## Workflow_cas
bart_wf_cas <- workflow() %>%
  add_model(bartmodel) %>%
  add_recipe(bike_recipe_cas)

## Fit bart_cas
fitted_bart_cas <- bart_wf_cas %>%
  fit(data = traindata_cas)
  

## predict
preds_reg <- predict(fitted_bart, new_data = testdata)
preds_cas <- predict(fitted_bart_cas, new_data = testdata)

preds1 <- exp(preds_reg -0.001)
preds2 <- exp(preds_cas -0.001)
preg_preds <- preds1 + preds2

# preds <- preds_reg + preds_cas
# 
# preg_preds <- exp(preds)

## Format the Predictions for Submission to Kaggle
kaggle_submission <-  preg_preds %>%
  bind_cols(., testdata) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out file
vroom_write(x=kaggle_submission, file="./bartcombined5.csv", delim=",")







