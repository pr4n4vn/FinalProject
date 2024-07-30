#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#


# Load necessary packages
library(plumber)
library(rpart)
library(caret)

# Load the best model
best_model <- readRDS("best_model.rds")

# Load data to determine default values
data <- read.csv("diabetes_012_health_indicators_BRFSS2015.csv")

# Preprocess the data
data$Diabetes_binary <- factor(ifelse(data$Diabetes_012 == 2, "Yes", "No"))


#Set the data into training and test.
set.seed(123)
train_index <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

train_control <- trainControl(method = "cv", number = 5, summaryFunction = mnLogLoss, classProbs = TRUE)

class_tree <- train(Diabetes_binary ~ ., 
                    data = train_data, 
                    method = "rpart", 
                    trControl = train_control, 
                    metric = "logLoss", 
                    tuneGrid = expand.grid(cp = seq(from = 0, to = 0.1, by = 0.001)))


# Define default values
default_values <- list(
  BMI = mean(data$BMI, na.rm = TRUE),
  HighBP = "No",
  HighChol = "No",
  Smoker = "No",
  CholCheck = "No",
  Stroke = "No",
  HeartDiseaseorAttack = "No",
  PhysActivity = "No",
  Fruits = "Yes",
  Veggies = "No",
  HvyAlcoholConsump = "Yes",
  AnyHealthcare = "No",
  NoDocbcCost = "Yes",
  GenHlth = "Good",
  MentHlth = 4,
  PhysHlth = 4,
  DiffWalk = "No",
  Sex = "Female",
  Age = "45-49",
  Education = "High School Grad",
  Income = "$50,000-$75,000"
)

#* @apiTitle Diabetes Prediction API

#* Predict diabetes probability
#* @param BMI Numeric, Body Mass Index
#* @param HighBP Character, "Yes" or "No" for high blood pressure
#* @param HighChol Character, "Yes" or "No" for high cholesterol
#* @param Smoker Character, "Yes" or "No" for smoking status
#* @param CholCheck Character, "Yes" or "No" for cholesterol status
#* @param Stroke Character, "Yes" or "No" for Stroke status
#* @param HeartDiseaseorAttack Character, "Yes" or "No" for disease or attack.
#* @param PhysActivity Character, "Yes" or "No" for physical activity
#* @param Fruits Character, "Yes" or "No" for consuming fruits
#* @param Veggies Character, "Yes" or "No" for consuming veggies
#* @param HvyAlcoholConsump Character, "Yes" or "No" for consuming alcohol
#* @param AnyHealthcare Character, "Yes" or "No" for 
#* @param NoDocbcCost Character, "Yes" or "No" for
#* @param GenHlth Character
#* @param MentHlth Numeric, number of days that required mental health check
#* @param PhysHlth Numeric, number of days that required physical health check
#* @param DiffWalk Character
#* @param Sex Character, "Female" or "Male"
#* @param Age Character, Age group
#* @param Education Character, Education level
#* @param Income Character, Income bracket


#* @post /pred
function(BMI = default_values$BMI, 
         HighBP = default_values$HighBP, 
         HighChol = default_values$HighChol, 
         Smoker = default_values$Smoker, 
         CholCheck = default_values$CholCheck,
         Stroke = default_values$Stroke,
         HeartDiseaseorAttack = default_values$HeartDiseaseorAttack,
         PhysActivity = default_values$PhysActivity,
         Fruits = default_values$Fruits,
         Veggies = default_values$Veggies,
         HvyAlcoholConsump = default_values$HvyAlcoholConsump,
         AnyHealthcare = default_values$AnyHealthcare,
         NoDocbcCost = default_values$NoDocbcCost,
         GenHlth = default_values$GenHlth,
         MentHlth = default_values$MentHlth,
         PhysHlth = default_values$PhysHlth,
         DiffWalk = default_values$DiffWalk,
         Sex = default_values$Sex,
         Age = default_values$Age,
         Education = default_values$Education,
         Income = default_values$Income
         ) {
  
  input_data <- data.frame(
    BMI = as.numeric(BMI),
    HighBP = factor(HighBP, levels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c("No", "Yes")),
    Smoker = factor(Smoker, levels = c("No", "Yes")),
    Age = factor(Age, levels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")),
    CholCheck = factor(CholCheck, levels = c("No", "Yes")),
    Stroke = factor(Stroke, levels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c("No","Yes")),
    PhysActivity = factor(PhysActivity, levels = c("No","Yes")),
    Fruits = factor(Fruits, levels = c("No","Yes")),
    Veggies = factor(Veggies, levels = c("No","Yes")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c("No","Yes")),
    AnyHealthcare = factor(AnyHealthcare, levels = c("No","Yes")),
    NoDocbcCost = factor(NoDocbcCost, levels = c("No","Yes")),
    MentHlth = as.numeric(MentHlth),
    PhysHlth = as.numeric(PhysHlth),
    GenHlth = factor(GenHlth, levels = 1:5, labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    DiffWalk = factor(DiffWalk, levels = c("No", "Yes")),
    Sex = factor(Sex, levels = c("Female","Male")),
    Age = factor(Age, levels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")),
    Education = factor(Education, levels = c("No Schooling", "Elementary", "Some High School", "High School Grad", "Some College", "College Grad")),
    Income = factor(Income, levels = c("<$10,000", "$10,000-$15,000", "$15,000-$20,000", "$20,000-$25,000", "$25,000-$35,000", "$35,000-$50,000", "$50,000-$75,000", ">$75,000"))
  )
  
  prediction <- predict(best_model, input_data, type = "prob")
  
  return(prediction)
}

#* API info
#* @get /info
function() {
  list(
    name = "Pranav Nair",
    url = "https://pr4n4vn.github.io/FinalProject/"
  )
}

# Example function calls:
# curl -X POST "http://localhost:8000/pred?BMI=25&HighBP=No&HighChol=No&Smoker=No&Age=45-49"
# curl -X POST "http://localhost:8000/pred?BMI=30&HighBP=Yes&HighChol=Yes&Smoker=Yes&Age=60-64"
# curl -X POST "http://localhost:8000/pred?BMI=22&HighBP=No&HighChol=No&Smoker=No&Age=18-24"
