#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#


library(plumber)


best_model <- readRDS("/best_model.rds")

diabetes_data <- read.csv("/diabetes_012_health_indicators_BRFSS2015.csv")

#define default values
default_values <- lapply(diabetes_data, function(column) {
  if(is.numeric(column)) {
    mean(column, na.rm = TRUE)
  } else {
    as.character(sort(table(column), decreasing = TRUE)[1])
  }
})


#* @apiTitle Diabetes Prediction API

#* Predict diabetes
#* @param HighBP
#* @param HighChol
#* @param CholCheck
#* @param BMI
#* @param Smoker
#* @param Stroke
#* @param HeartDiseaseorAttack
#* @param PhysActivity
#* @param Fruits
#* @param Veggies
#* @param HvyAlcoholConsump
#* @param AnyHealthcare
#* @param NoDocbcCost
#* @param GenHlth
#* @param MentHlth
#* @param PhysHlth
#* @param DiffWalk
#* @param Sex
#* @param Age
#* @param Education
#* @param Income
#* @post /pred
function(HighBP = default_values$HighBP, HighChol = default_values$HighChol, CholCheck = default_values$CholCheck,
         BMI = default_values$BMI, Smoker = default_values$Smoker, Stroke = default_values$Stroke, 
         HeartDiseaseorAttack = default_values$HeartDiseaseorAttack, PhysActivity = default_values$PhysActivity,
         Fruits = default_values$Fruits, Veggies = default_values$Veggies, HvyAlcoholConsump = default_values$HvyAlcoholConsump,
         AnyHealthcare = default_values$AnyHealthcare, NoDocbcCost = default_values$NoDocbcCost, GenHlth = default_values$GenHlth,
         MentHlth = default_values$MentHlth, PhysHlth = default_values$PhysHlth, DiffWalk = default_values$DiffWalk,
         Sex = default_values$Sex, Age = default_values$Age, Education = default_values$Education, Income = default_values$Income) {
  
  new_data <- data.frame(HighBP, HighChol, CholCheck, BMI, Smoker, Stroke, HeartDiseaseorAttack, PhysActivity,
                         Fruits, Veggies, HvyAlcoholConsump, AnyHealthcare, NoDocbcCost, GenHlth, MentHlth, 
                         PhysHlth, DiffWalk, Sex, Age, Education, Income)
  
  predict(best_model, new_data, type = "response")
}

#* Get info about the API
#* @get /info
function() {
  list(
    name = "Pranav Nair",
    url = "https://pr4n4vn.github.io/FinalProject/EDA.html"
  )
}