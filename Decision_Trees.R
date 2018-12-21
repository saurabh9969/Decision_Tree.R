## Business understanding

# Our example concerns a big company that wants to understand 
# why some of their best and most experienced employees are leaving prematurely. 
# The company also wishes to predict which valuable employees will leave next.


## Analytic solution 

# We have two goals: first, we want to understand why valuable employees leave, 
# and second, we want to predict who will leave next.
# 
# Therefore, we propose to work with the HR department to gather relevant data 
# about the employees and to communicate the significant effect that could explain 
# and predict employees' departure.

# For our 15 000 employees we know: 
#   
# satisfaction level
# latest evaluation (yearly)
# number of project worked/working on
# average monthly hours
# time spend in the company (in years)
# work accident (within the past 2 years)
# promotion within the past 5 years
# department
# salary

## Analytical Base Table

# This is the database from the HR department: 
# (Note that it doesn't take into account the person that have been fired, 
# transferred or hired in the past year...)

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvis)
library(corrplot)
library(DT)


setwd("C:\\Users\\SG_PERSONAL\\Downloads\\Citrix\\R\\Decision_Trees")
hr = read.csv('HR_comma_sep.csv')

head(hr)


### Summary

summary(hr)

## Data quality report

# This table describe the characteristics of each features. 
# We can see different statistical measures of central tendency and variation. 
# For example we can see that our attrition rate is equal to 24%, 
# the satisfaction level is around 62% and the performance average is around 71%. 
# We see that on average people work on 3 to 4 projects a year and about 200 hours per months.

## First visualisations

### Graph

# This graph present the correlations between each variables. 
# The size of the bubbles reveal the significance of the correlation, 
# while the colour present the direction (either positive or negative).

HR_correlation <- hr %>% select(satisfaction_level:promotion_last_5years)
M <- cor(HR_correlation)
corrplot(M, method="circle")

# On average people who leave have a low satisfaction level

## Who is leaving?

# Let's create a data frame with only the people that have left the company, 
# so we can visualise what is the distribution of each features:

hr_hist <- hr %>% filter(left==1)
par(mfrow=c(1,3))
hist(hr_hist$satisfaction_level,col="#3090C7", main = "Satisfaction level") 
hist(hr_hist$last_evaluation,col="#3090C7", main = "Last evaluation")
hist(hr_hist$average_montly_hours,col="#3090C7", main = "Average montly hours")

# We can see why we don't want to retain everybody. 
# Some people don't work well as we can see from their evaluation, 
# but clearly there are also many good workers that leave.

par(mfrow=c(1,2))
hist(hr_hist$Work_accident,col="#3090C7", main = "Work accident")
plot(hr_hist$salary,col="#3090C7", main = "Salary")

# In the total of 15 000 employees that compose our database, 
# here are the people that have left:

hr_leaving_people <- hr %>% filter(left==1)
nrow(hr_leaving_people)

# More problematic, here are the total of employees that received an evaluation above average, 
# or spend at least four years in the company, 
# or were working/worked on more than 5 projects and still have left the company. 
# **These are the people company should have retained.**

hr_good_leaving_people <- hr_leaving_people %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
nrow(hr_good_leaving_people)

## Why good people leave?

# Let's re-use the data table created above that contain only the most valuable employees and see why they tend to leave.

hr_good_leaving_people <- hr %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
hr_good_people_select <- hr_good_leaving_people %>% select(satisfaction_level, number_project: promotion_last_5years)
M <- cor(hr_good_people_select)
corrplot(M, method="circle")

# Here it's much clearer. 
# On average valuable employees that leave are not satisfayed, 
# work on many projects, spend many hours in the company each month and aren't promoted.

summary(hr_good_leaving_people)

# Modeling 

# Now we want to predict which valuable employe will leave next.

## Select database

# Let's use the same database than above where we kept the most valuable employees. 
# Here is the summary of that database.

hr_model <- hr %>% filter(last_evaluation >= 0.70 | time_spend_company >= 4 | number_project > 5)
summary(hr_model)


####################### CART ###########################################

library(rpart)
library(rpart.plot)

# train the model 

rpartmodel<- rpart(left~., data=hr_model, method="class", minbucket=25)

plot(rpartmodel)
text(rpartmodel)

rpart.plot(rpartmodel)

# make predictions
predictions <- predict(rpartmodel,hr_model,type = "class")
hr_model_tree <- cbind(hr_model,predictions)


library(caret)
# summarize results
hr_model_tree$predictions <- as.factor(hr_model_tree$predictions)
hr_model_tree$left <- as.factor(hr_model_tree$left)

confusionMatrix<- confusionMatrix(hr_model_tree$predictions,hr_model_tree$left)
confusionMatrix

################## On Test and Train Set ###################

set.seed(100)

# Keep some data to test again the final model
inTraining <- createDataPartition(hr_model$left, p = .75, list = FALSE)
training <- hr_model[ inTraining,]
testing  <- hr_model[-inTraining,]

model<- rpart(left~., data=training ,method="class", minbucket=25)

# make predictions
predictions<- predict(model, testing, type = "class")
hr_model_tree<- cbind(testing, predictions)

# summarize results
confusionMatrix<- confusionMatrix(hr_model_tree$predictions,hr_model_tree$left)
confusionMatrix

############### Random Forest ##########################

library(randomForest)

training$left <- as.factor(training$left)
testing$left <- as.factor(testing$left)

RF_Model <- randomForest(left~., data=training, ntree=200, nodesize=25)

plot(RF_Model)
text(RF_Model)

# Make predictions
PredictForest = predict(RF_Model, newdata = testing)
confusionMatrix(testing$left,PredictForest)
