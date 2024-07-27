#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(tidyverse)
library(caret)


best_model <- readRDS("best_model.rds")

#Load in the data
data <- read.csv("diabetes_012_health_indicators_BRFSS2015.csv")


#clean the data shown in the other 
data <- data %>%
  mutate(Diabetes_binary = factor(ifelse(Diabetes_012 == 2, "Yes", "No")),  # Create binary diabetes variable
         HighBP = factor(HighBP, levels = c(0, 1), labels = c("No", "Yes")),
         HighChol = factor(HighChol, levels = c(0, 1), labels = c("No", "Yes")),
         CholCheck = factor(CholCheck, levels = c(0, 1), labels = c("No", "Yes")),
         Smoker = factor(Smoker, levels = c(0, 1), labels = c("No", "Yes")),
         Stroke = factor(Stroke, levels = c(0, 1), labels = c("No", "Yes")),
         HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c(0, 1), labels = c("No", "Yes")),
         PhysActivity = factor(PhysActivity, levels = c(0, 1), labels = c("No", "Yes")),
         Fruits = factor(Fruits, levels = c(0, 1), labels = c("No", "Yes")),
         Veggies = factor(Veggies, levels = c(0, 1), labels = c("No", "Yes")),
         HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0, 1), labels = c("No", "Yes")),
         AnyHealthcare = factor(AnyHealthcare, levels = c(0, 1), labels = c("No", "Yes")),
         NoDocbcCost = factor(NoDocbcCost, levels = c(0, 1), labels = c("No", "Yes")),
         DiffWalk = factor(DiffWalk, levels = c(0, 1), labels = c("No", "Yes")),
         Sex = factor(Sex, levels = c(0, 1), labels = c("Female", "Male")),
         Age = factor(Age, levels = 1:13, labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")),
         Education = factor(Education, levels = 1:6, labels = c("No Schooling", "Elementary", "Some High School", "High School Grad", "Some College", "College Grad")),
         Income = factor(Income, levels = 1:8, labels = c("<$10,000", "$10,000-$15,000", "$15,000-$20,000", "$20,000-$25,000", "$25,000-$35,000", "$35,000-$50,000", "$50,000-$75,000", ">$75,000"))
  )


#define default values
default_values <- list(
  BMI = mean(data$BMI, na.rm = TRUE),
  HighBP = "No",
  HighChol = "No",
  Smoker = "No",
  Age = "45-49"
)

#* @apiTitle Diabetes Prediction API

#* Predict diabetes probability
#* @param BMI Numeric, Body Mass Index
#* @param HighBP Character, "Yes" or "No" for high blood pressure
#* @param HighChol Character, "Yes" or "No" for high cholesterol
#* @param Smoker Character, "Yes" or "No" for smoking status
#* @param Age Character, Age group
#* @post /pred
#Now to start the function that takes in any predictor variables as the input.
function(BMI = default_values$BMI, HighBP = default_values$HighBP, HighChol = default_values$HighChol, Smoker = default_values$Smoker, Age = default_values$Age){
  input_data <- data.frame(
    BMI = as.numeric(BMI),
    HighBP = factor(HighBP, levels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c("No", "Yes")),
    Smoker = factor(Smoker, levels = c("No", "Yes")),
    Age = factor(Age, levels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"))
    )
    
    prediction <- predict(best_model, input_data, type = "prob")
    
    return(prediction)
}

#* API info
#* @get /info
function() {
  list(
    name = "Pranav Nair",
    url = "https://pr4n4vn.github.io/FinalProject/EDA.html"
  )
}
