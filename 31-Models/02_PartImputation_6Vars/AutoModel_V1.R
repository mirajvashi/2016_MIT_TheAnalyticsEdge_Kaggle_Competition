AutoModel <- function(DF_INPUT, SEED=1)
{
###############################################################################################
## PURPOSE: TEMPLATE R-SCRIPT TO AUTOMATE MODEL BUILDING & ACCURACY CHECK
## USAGE: CUSTOMIZE EACH SECTION FOR SPECIFIC DATA ANALYSIS NEED
## TECHNIQUES: GLM, CART & RANDOM FORREST
###############################################################################################

## LOAD REQUIRED LIBRARIES
library(caTools)
library(rpart)
library(randomForest)
library(ROCR)

## INITIALIZE VARIABLES

ModelTypes = c("glm", "CART", "RandomForest")
SplRatio <- c(0.50,0.55,0.60,0.65,0.70,0.75,0.80)
RandSeed <- ifelse(SEED != 1, SEED, 13)
Threshold <- c(0.40,0.45,0.50,0.55,0.60)
TotalRows <- ((length(ModelTypes)-1) * length(SplRatio)) + (length(SplRatio) * length(Threshold))
dfInput = DF_INPUT
rIndex = 1

## INITIALIZE OUTPUT DATA FRAME

dfOutput = data.frame(matrix(NA, nrow=TotalRows, ncol=6))
colnames(dfOutput) = c("Model.Technique", "SplitRatio", "Threshold", "AUC", "TrainingSet_Accuracy", "TestingSet_Accuracy")

## BUILD & EVALUATE MODELS

for (i in 1:length(SplRatio))
{
    set.seed(RandSeed)
    
    ####: Needs to be customized
    splt = sample.split(dfInput$Party, SplitRatio = SplRatio[i])
    dfTrain = subset(dfInput, splt == TRUE)
    dfTest = subset(dfInput, splt == FALSE)
    
    ####: Check for required customization before each run
    ####: If Model parameters are changed - Upgrade Version Number in .R & Output file
    glmModel = glm(Party ~ ., data = dfTrain, family = "binomial")
    cartModel = rpart(Party ~ ., data = dfTrain, method = "class")
    rfModel = randomForest(Party ~ ., data = dfTrain)
    
    for(i1 in 1:length(ModelTypes))
    {
        if(ModelTypes[i1] == "glm")
        {
            predTrain = predict(glmModel, type = "response")
            predTest = predict(glmModel, newdata = dfTest, type = "response")
            
            for(i2 in 1:length(Threshold))
            {
                dfOutput[rIndex,1] = ModelTypes[i1]
                dfOutput[rIndex,2] = SplRatio[i]
                dfOutput[rIndex,3] = Threshold[i2]
                ####: Check for required customization before each run
                dfOutput[rIndex,4] = as.numeric(performance(prediction(predTest, dfTest$Party), "auc")@y.values)
                m = as.matrix(table(dfTrain$Party, predTrain > Threshold[i2]))
                dfOutput[rIndex,5] = (m[1,1] + m[2,2]) / nrow(dfTrain)
                m = as.matrix(table(dfTest$Party, predTest > Threshold[i2]))
                dfOutput[rIndex,6] = (m[1,1] + m[2,2]) / nrow(dfTest)
                rIndex = rIndex + 1
            }
        }
        
        if(ModelTypes[i1] == "CART")
        {
            predTrain = predict(cartModel, type = "class")
            predTest = predict(cartModel, newdata = dfTest, type = "class")
            
            dfOutput[rIndex,1] = ModelTypes[i1]
            dfOutput[rIndex,2] = SplRatio[i]
            dfOutput[rIndex,3] = ""
            dfOutput[rIndex,4] = ""
            ####: Check for required customization before each run
            m = as.matrix(table(dfTrain$Party, predTrain))
            dfOutput[rIndex,5] = (m[1,1] + m[2,2]) / nrow(dfTrain)
            m = as.matrix(table(dfTest$Party, predTest))
            dfOutput[rIndex,6] = (m[1,1] + m[2,2]) / nrow(dfTest)
            rIndex = rIndex + 1
        }
        
        if(ModelTypes[i1] == "RandomForest")
        {
            predTrain = predict(rfModel, type = "class")
            predTest = predict(rfModel, newdata = dfTest, type = "class")
            
            dfOutput[rIndex,1] = ModelTypes[i1]
            dfOutput[rIndex,2] = SplRatio[i]
            dfOutput[rIndex,3] = ""
            dfOutput[rIndex,4] = ""
            ####: Check for required customization before each run
            m = as.matrix(table(dfTrain$Party, predTrain))
            dfOutput[rIndex,5] = (m[1,1] + m[2,2]) / nrow(dfTrain)
            m = as.matrix(table(dfTest$Party, predTest))
            dfOutput[rIndex,6] = (m[1,1] + m[2,2]) / nrow(dfTest)
            rIndex = rIndex + 1
        }
    }
}
## WRITE THE OUTPUT TO CSV FILE
    write.csv(dfOutput, "AutoModel_V1_Output.csv", row.names = FALSE)
}