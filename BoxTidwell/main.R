# Examples taken from the following sources:
# 1. https://stats.oarc.ucla.edu/r/dae/logit-regression/
# 1a. Easy example to follow logistic regression with the data
# 2. https://r4ds.github.io/bookclub-islr/addendum---logistic-regression-assumptions.html
# 2a. Working with the boxTidwell function to check linearity in logit
library(aod)
library(ggplot2)
library(car)

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)
## prepare logistic regression
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)

# Predict the probability (p) of diabete positivity
probabilities <- predict(mylogit, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")


## test boxTidwell
