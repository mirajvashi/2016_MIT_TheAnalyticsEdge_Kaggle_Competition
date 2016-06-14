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
## VAR. SET: FEATURES SELECTED USING - RANDOM FOREST 5000 TRESS !
## ############################################################################################

## LOAD REQUIRED LIBRARIES
library(caTools)
library(rpart)
library(randomForest)
library(ROCR)

## INITIALIZE VARIABLES

BaselineModel = "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163"
NextVariable =  c("Gender", "Q110740", "Q103293", "Q104996", "Q114748", "Q115777", "Q120194", "Q115899", "Q102687", "Q116953", "Q105655", "Q112270", "Q118232", "Q122770", "Q120379", "Q119334", "Q120978", "Q115390", "Q114961", "Q115195", "Q120012", "Q98078", "Q119851", "Q112478", "Q113584", "Q99982", "Q116448", "Q105840", "Q117193", "Q120472", "Q121011", "Q124122", "Q106042", "Q118237", "Q96024", "Q108855", "Q111848", "Q114386", "Q122769", "Q106997", "Q118892", "Q98578", "Q116797", "Q107869", "Q120014", "Q102906", "Q117186", "Q118117", "Q111580", "Q100680", "Q100689", "Q106389", "Q116197", "Q124742", "Q116881", "Q108950", "Q118233", "Q101162", "Q109367", "Q114517", "Q108342", "Q98869", "Q108856", "Q101596", "Q99480", "Q116441", "Q102289", "Q111220", "Q108754", "Q108343", "Q113992", "Q121699", "Q122120", "Q113583", "Q106272", "Q102089", "Q123621", "Q114152", "Q102674", "Q119650", "Q106388", "Q100010", "Q115602", "Q122771", "Q106993", "Q100562", "Q115610", "Q112512", "Q116601", "Q107491", "Q108617", "Q121700", "Q99581", "Q99716", "Q98059", "Q120650", "Q123464")
FormulaString = ""
##PredictionFormula = c("Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Gender", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q110740", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q103293", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q104996", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q114748", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q115777", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q120194", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q115899", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q102687", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q116953", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q105655", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q112270", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q118232", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q122770", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q120379", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q119334", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q120978", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q115390", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q114961", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q115195", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q120012", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q98078", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q119851", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q112478", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q113584", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q99982", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q116448", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q105840", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q117193", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q120472", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q121011", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q124122", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q106042", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q118237", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q96024", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q108855", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q111848", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q114386", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q122769", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q106997", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q118892", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q98578", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q116797", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q107869", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q120014", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q102906", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q117186", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q118117", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q111580", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q100680", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q100689", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q106389", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q116197", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q124742", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q116881", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q108950", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q118233", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q101162", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q109367", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q114517", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q108342", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q98869", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q108856", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q101596", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q99480", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q116441", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q102289", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q111220", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q108754", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q108343", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q113992", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q121699", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q122120", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q113583", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q106272", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q102089", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q123621", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q114152", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q102674", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q119650", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q106388", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q100010", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q115602", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q122771", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q106993", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q100562", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q115610", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q112512", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q116601", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q107491", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q108617", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q121700", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q99581", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q99716", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q98059", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q120650", "Party ~ YOB + Income + EducationLevel + Q109244 + HouseholdStatus + Q115611 + Q113181 + Q98197 + Q101163 + Q123464")
##ModelTypes = c("glm", "CART", "RandomForest")
ModelTypes = c("glm")
SplRatio <- c(0.999)
RandSeed <- ifelse(SEED != 1, SEED, 13)
Threshold <- c(0.50)
TotalRows <- (((length(ModelTypes)-1) * length(SplRatio)) + (length(SplRatio) * length(Threshold)))*length(NextVariable)
dfInput = DF_INPUT
rIndex = 1

## INITIALIZE OUTPUT DATA FRAME

dfOutput = data.frame(matrix(NA, nrow=TotalRows, ncol=9))
colnames(dfOutput) = c("Model.Technique", "SplitRatio", "Threshold", "Training.AUC", "Test.AUC", "TrainingSet.Accuracy", "TestSet.Accuracy", "Next.Variable", "Prediction.Formula")

## BUILD & EVALUATE MODELS

for(i3 in 0:length(NextVariable))
{
    if(i3==0)
    {
        predFormula = as.formula(BaselineModel)
    }
    else
    {
        FormulaString = paste(BaselineModel, " + ", NextVariable[i3], sep = "")
        predFormula = as.formula(FormulaString)
    }
        
    for (i in 1:length(SplRatio))
    {
        set.seed(RandSeed)
        
        ####: Needs to be customized
        splt = sample.split(dfInput$Party, SplitRatio = SplRatio[i])
        dfTrain = subset(dfInput, splt == TRUE)
        dfTest = subset(dfInput, splt == FALSE)
        
        ####: Check for required customization before each run
        glmModel = glm(predFormula, data = dfTrain, family = "binomial")
        #cartModel = rpart(predFormula, data = dfTrain, method = "class")
        #rfModel = randomForest(predFormula, data = dfTrain, ntree = 1500)
        
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
                    dfOutput[rIndex,4] = as.numeric(performance(prediction(predTrain, dfTrain$Party), "auc")@y.values)
                    dfOutput[rIndex,6] = (m[1,1] + m[2,2]) / nrow(dfTrain)
                    if(i3>0) 
                        dfOutput[rIndex,8] = NextVariable[i3]
                    dfOutput[rIndex,9] = FormulaString
                    
                    if(SplRatio[i] < 1)
                    {
                      dfOutput[rIndex,5] = as.numeric(performance(prediction(predTest, dfTest$Party), "auc")@y.values)
                      m = as.matrix(table(dfTest$Party, predTest > Threshold[i2]))
                      dfOutput[rIndex,7] = (m[1,1] + m[2,2]) / nrow(dfTest)
                    }
                    rIndex = rIndex + 1
                }
            }
            
            if(ModelTypes[i1] == "CART")
            {
                predTrain = predict(cartModel, type = "class")
                predTest = predict(cartModel, newdata = dfTest, type = "class")
                predTrainProb = predict(cartModel, type = "prob")
                predTestProb = predict(cartModel, newdata = dfTest, type = "prob")
                
                ####: Check for required customization before each run
                dfOutput[rIndex,1] = ModelTypes[i1]
                dfOutput[rIndex,2] = SplRatio[i]
                dfOutput[rIndex,3] = ""
                dfOutput[rIndex,4] = as.numeric(performance(prediction(predTrainProb[ ,2], dfTrain$Party), "auc")@y.values)
                m = as.matrix(table(dfTrain$Party, predTrain))
                dfOutput[rIndex,6] = (m[1,1] + m[2,2]) / nrow(dfTrain)
                dfOutput[rIndex,8] = NextVariable[i3]
                dfOutput[rIndex,9] = FormulaString
                
                if(SplRatio[i] < 1)
                {
                  dfOutput[rIndex,5] = as.numeric(performance(prediction(predTestProb[ ,2], dfTest$Party), "auc")@y.values)
                  m = as.matrix(table(dfTest$Party, predTest))
                  dfOutput[rIndex,7] = (m[1,1] + m[2,2]) / nrow(dfTest)
                }
                rIndex = rIndex + 1
            }
            
            if(ModelTypes[i1] == "RandomForest")
            {
                predTrain = predict(rfModel, type = "class")
                predTest = predict(rfModel, newdata = dfTest, type = "class")
                predTrainProb = predict(rfModel, type = "prob")
                predTestProb = predict(rfModel, newdata = dfTest, type = "prob")
                
                ####: Check for required customization before each run
                dfOutput[rIndex,1] = ModelTypes[i1]
                dfOutput[rIndex,2] = SplRatio[i]
                dfOutput[rIndex,3] = ""
                dfOutput[rIndex,4] = as.numeric(performance(prediction(predTrainProb[ ,2], dfTrain$Party), "auc")@y.values)
                m = as.matrix(table(dfTrain$Party, predTrain))
                dfOutput[rIndex,6] = (m[1,1] + m[2,2]) / nrow(dfTrain)
                dfOutput[rIndex,8] = NextVariable[i3]
                dfOutput[rIndex,9] = FormulaString
                
                if(SplRatio[i] < 1)
                {
                  dfOutput[rIndex,5] = as.numeric(performance(prediction(predTestProb[ ,2], dfTest$Party), "auc")@y.values)
                  m = as.matrix(table(dfTest$Party, predTest))
                  dfOutput[rIndex,7] = (m[1,1] + m[2,2]) / nrow(dfTest)
                }
                rIndex = rIndex + 1
            }
        }
    }
}
## WRITE THE OUTPUT TO CSV FILE
    write.csv(dfOutput, "Auto_Model_Output.csv", row.names = FALSE)
}