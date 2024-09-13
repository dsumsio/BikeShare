library(tidyverse)
library(tidymodels)
library(vroom)
library(GGally)
library(corrplot)
library(patchwork)

## Reading in the Data
traindata <- vroom("train.csv")
testdata <- vroom("test.csv")

nrow(traindata)

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











