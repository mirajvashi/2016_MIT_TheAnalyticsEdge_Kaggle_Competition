FactorVariableSignificance <- function(dfMain)
{
## This function summarizes the variables from input data frame 
## Build summary data frame 
    dfSummary = data.frame(colnames(dfMain))
    colIndex <- length(colnames(dfMain))
    TotalRows = nrow(dfMain)
    
## Build vectors with default values
    DEMO.NO = rep(-1, times = colIndex)
    DEMO.YES = rep(-1, times = colIndex)
    REPUB.NO = rep(-1, times = colIndex)
    REPUB.YES = rep(-1, times = colIndex)
    DEMO.NO.PERCENT = rep(-1, times = colIndex)
    DEMO.YES.PERCENT = rep(-1, times = colIndex)
    REPUB.NO.PERCENT = rep(-1, times = colIndex)
    REPUB.YES.PERCENT = rep(-1, times = colIndex)
    
## Populate vectors with summary statistics
    for(i in 1:colIndex)
    {
        if(class(dfMain[ ,i]) == "factor")
        {
          m = as.matrix(table(dfMain$Party, dfMain[ ,i]))
          DEMO.NO[i] = m[1,1]
          DEMO.YES[i] = m[1,2]
          REPUB.NO[i] = m[2,1]
          REPUB.YES[i] = m[2,2]
          DEMO.NO.PERCENT[i] = (m[1,1]*100)/TotalRows
          DEMO.YES.PERCENT[i] = (m[1,2]*100)/TotalRows
          REPUB.NO.PERCENT[i] = (m[2,1]*100)/TotalRows
          REPUB.YES.PERCENT[i] = (m[2,2]*100)/TotalRows
        }
    }

## Augment summary data frame with additional columns  
    dfSummary$DEMO.NO = DEMO.NO
    dfSummary$DEMO.YES = DEMO.YES
    dfSummary$REPUB.NO = REPUB.NO
    dfSummary$REPUB.YES = REPUB.YES
    dfSummary$DEMO.NO.PERCENT = DEMO.NO.PERCENT
    dfSummary$DEMO.YES.PERCENT = DEMO.YES.PERCENT
    dfSummary$REPUB.NO.PERCENT = REPUB.NO.PERCENT
    dfSummary$REPUB.YES.PERCENT = REPUB.YES.PERCENT
    
## Write the out to Working Directory
    write.csv(dfSummary, file = "FactorVariableSignificance.csv")
    
## Return the Summary data frame to calling Program
    dfSummary
}