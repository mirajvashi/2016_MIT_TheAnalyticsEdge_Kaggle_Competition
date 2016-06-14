AutoModel <- function(DF_INPUT, SEED=1)
{
###############################################################################################
## PURPOSE: TEMPLATE R-SCRIPT TO AUTOMATE MODEL BUILDING & ACCURACY CHECK
## USAGE: CUSTOMIZE EACH SECTION FOR SPECIFIC DATA ANALYSIS NEED
## TECHNIQUES: GLM, CART & RANDOM FORREST
###############################################################################################

## ############################################################################################
## MODEL LOGIC DESCRIPTION
## DATA sET: FULLY IMPUTED - MICE PMM METHOD
## VAR. SET: FEATURES SELECTED USING BORUTA | SCORE = 4 FEATURES
## ############################################################################################

## LOAD REQUIRED LIBRARIES
library(caTools)
library(rpart)
library(randomForest)
library(ROCR)

## INITIALIZE VARIABLES

PredictionFormula = c("Party ~ YOB + Income", "Party ~ YOB + Income + EducationLevel", "Party ~ YOB + Income + EducationLevel + Q109244", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Gender", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197")
ModelTypes = c("glm", "CART", "RandomForest")
SplRatio <- c(0.99)
RandSeed <- ifelse(SEED != 1, SEED, 13)
Threshold <- c(0.50)
TotalRows <- (((length(ModelTypes)-1) * length(SplRatio)) + (length(SplRatio) * length(Threshold)))*length(PredictionFormula)
dfInput = DF_INPUT
rIndex = 1

## INITIALIZE OUTPUT DATA FRAME

dfOutput = data.frame(matrix(NA, nrow=TotalRows, ncol=7))
colnames(dfOutput) = c("Model.Technique", "SplitRatio", "Threshold", "AUC", "TrainingSet_Accuracy", "TestingSet_Accuracy", "Prediction Formula")

## BUILD & EVALUATE MODELS

for(i3 in 1:length(PredictionFormula))
{
    predFormula = as.formula(PredictionFormula[i3])
    
    for (i in 1:length(SplRatio))
    {
        set.seed(RandSeed)
        
        ####: Needs to be customized
        splt = sample.split(dfInput$Party, SplitRatio = SplRatio[i])
        dfTrain = subset(dfInput, splt == TRUE)
        dfTest = subset(dfInput, splt == FALSE)
        
        ####: Check for required customization before each run
        glmModel = glm(predFormula, data = dfTrain, family = "binomial")
        cartModel = rpart(predFormula, data = dfTrain, method = "class")
        rfModel = randomForest(predFormula, data = dfTrain, ntree = 1500)
        
        for(i1 in 1:length(ModelTypes))
        {
            if(ModelTypes[i1] == "glm")
            {
                predTrain = predict(glmModel, type = "response")
                predTest = predict(glmModel, newdata = dfTest, type = "response")
                
                ####: Check for required customization before each run
                for(i2 in 1:length(Threshold))
                {
                    dfOutput[rIndex,1] = ModelTypes[i1]
                    dfOutput[rIndex,2] = SplRatio[i]
                    dfOutput[rIndex,3] = Threshold[i2]
                    m = as.matrix(table(dfTrain$Party, predTrain > Threshold[i2]))
                    dfOutput[rIndex,5] = (m[1,1] + m[2,2]) / nrow(dfTrain)
                    dfOutput[rIndex,7] = PredictionFormula[i3]
                    
                    if(SplRatio[i] < 1)
                    {
                      dfOutput[rIndex,4] = as.numeric(performance(prediction(predTest, dfTest$Party), "auc")@y.values)
                      m = as.matrix(table(dfTest$Party, predTest > Threshold[i2]))
                      dfOutput[rIndex,6] = (m[1,1] + m[2,2]) / nrow(dfTest)
                    }
                    rIndex = rIndex + 1
                }
            }
            
            if(ModelTypes[i1] == "CART")
            {
                predTrain = predict(cartModel, type = "class")
                predTest = predict(cartModel, newdata = dfTest, type = "class")
                predTestProb = predict(cartModel, newdata = dfTest)
                
                ####: Check for required customization before each run
                dfOutput[rIndex,1] = ModelTypes[i1]
                dfOutput[rIndex,2] = SplRatio[i]
                dfOutput[rIndex,3] = ""
                m = as.matrix(table(dfTrain$Party, predTrain))
                dfOutput[rIndex,5] = (m[1,1] + m[2,2]) / nrow(dfTrain)
                dfOutput[rIndex,7] = PredictionFormula[i3]
                
                if(SplRatio[i] < 1)
                {
                  #dfOutput[rIndex,4] = as.numeric(performance(prediction(predTestProb[ ,2], dfTest$Party), "auc")@y.values)
                  m = as.matrix(table(dfTest$Party, predTest))
                  dfOutput[rIndex,6] = (m[1,1] + m[2,2]) / nrow(dfTest)
                }
                rIndex = rIndex + 1
            }
            
            if(ModelTypes[i1] == "RandomForest")
            {
                predTrain = predict(rfModel, type = "class")
                predTest = predict(rfModel, newdata = dfTest, type = "class")
                predTestProb = predict(rfModel, newdata = dfTest)
                
                ####: Check for required customization before each run
                dfOutput[rIndex,1] = ModelTypes[i1]
                dfOutput[rIndex,2] = SplRatio[i]
                dfOutput[rIndex,3] = ""
                m = as.matrix(table(dfTrain$Party, predTrain))
                dfOutput[rIndex,5] = (m[1,1] + m[2,2]) / nrow(dfTrain)
                dfOutput[rIndex,7] = PredictionFormula[i3]
                
                if(SplRatio[i] < 1)
                {
                  #dfOutput[rIndex,4] = as.numeric(performance(prediction(predTestProb[ ,2], dfTest$Party), "auc")@y.values)
                  m = as.matrix(table(dfTest$Party, predTest))
                  dfOutput[rIndex,6] = (m[1,1] + m[2,2]) / nrow(dfTest)
                }
                rIndex = rIndex + 1
            }
        }
    }
}
## WRITE THE OUTPUT TO CSV FILE
    write.csv(dfOutput, "AutoModelOutput.csv", row.names = FALSE)
}