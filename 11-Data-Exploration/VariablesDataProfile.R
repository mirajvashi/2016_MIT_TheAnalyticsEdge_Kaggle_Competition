VariablesDataProfile <- function(dfMain)
{
## This function summarizes the variables from input data frame 
## Build summary data frame 
    dfSummary = data.frame(colnames(dfMain))
    Rows <- length(colnames(dfMain))
    
## Build vectors with default values
    NACount = rep(-1, times = Rows)
    LevelsCount = rep(-1, times = Rows)
    VarClass = rep("", times = Rows)

## Populate vectors with summary statistics
    for(i in 1:Rows)
    {
        NACount[i] = sum(is.na(dfMain[ ,i]))
        VarClass[i] = class(dfMain[ ,i])
        if(VarClass[i] == "factor")
          LevelsCount[i] = length(levels(as.factor(dfMain[ ,i])))
        else
          LevelsCount[i] = 0
    }

## Augment summary data frame with additional columns  
    dfSummary$Class = VarClass
    dfSummary$NACount = NACount
    dfSummary$LevelsCount = LevelsCount

## Provide use-friendly column names
    colnames(dfSummary) = c("Variable.Name", "Variable.Class", "NA Count", "Levels Count")

## Write the out to Working Directory
    write.csv(dfSummary, file = "VariablesDataProfile.csv")
    
## Return the Summary data frame to calling Program
    dfSummary
}