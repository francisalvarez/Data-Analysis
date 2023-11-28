# Examples taken from the following sources:
# 1. https://stats.oarc.ucla.edu/r/dae/logit-regression/
# 1a. Easy example to follow logistic regression with the data
# 2. https://r4ds.github.io/bookclub-islr/addendum---logistic-regression-assumptions.html
# 2a. Working with the boxTidwell function to check linearity in logit
# 3. Box Tidwell example for logistic regression
# https://github.com/kennethleungty/Logistic-Regression-Assumptions/blob/main/Box-Tidwell-Test-in-R.ipynb
library(aod)
library(ggplot2)
library(car)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)
## prepare logistic regression
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)

# Predict the probability (p) of diabete positivity
probabilities <- predict(mylogit, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
accuracy = sum(mydata$admit == predicted.classes)/length(predicted.classes)

# Select only numeric predictors
dat <- mydata %>% select_if(is.numeric) 
predictors <- colnames(dat)
# Bind the logit and tidying the data for plot
mydata2 <- dat %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

# plotting
ggplot(mydata2, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

## test boxTidwell
boxTidwell(formula = admit ~ gre + gpa, 
           other.x = ~ rank, 
           data = mydata)
## Since all p-values are above 0.05 (each variable is not statistically significant)
## This means that there is no evidence supporting linearity in the gre feature
# or the gpa feature and the assumption is valid
