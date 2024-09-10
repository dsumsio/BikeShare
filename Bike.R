library(tidyverse)
library(tidymodels)
library(vroom)
library(GGally)
library(corrplot)
library(patchwork)

data <- vroom("train.csv")


corr <- corrplot(cor(data[sapply(data, is.numeric)]), type = "upper")

humid <- ggplot(data, mapping=aes(x=humidity, y = count)) +
  geom_point() +
  geom_smooth(se=F)

temp <- ggplot(data, mapping=aes(x=temp, y = count)) +
  geom_point() +
  geom_smooth(se=F)

weather <- ggplot(data, mapping = aes(x = weather)) +
  geom_bar()

season <- ggplot(data, mapping=aes(x=season)) +
  geom_bar()

holiday <- ggplot(data, mapping = aes(x = holiday)) +
  geom_bar()


(holiday + weather) / (temp + humid)

sum(data$weather == 4)

ggplot(data, mapping = aes(x = windspeed, y = count)) +
  geom_point()
