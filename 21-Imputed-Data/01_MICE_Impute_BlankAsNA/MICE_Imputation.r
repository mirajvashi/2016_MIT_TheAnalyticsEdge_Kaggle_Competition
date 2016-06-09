## MICE IMPUTATION STE
dfTrain = read.csv("train2016.csv", na.strings=c("","NA"))
dfTest = read.csv("test2016.csv", na.strings=c("","NA"))
dfTrainClean1 = subset(dfTrain, (YOB >= 1900 & YOB <=2016) | (is.na(YOB)))
varPartyInTrain = varParty
varUserIdInTrain = dfTrain$USER_ID
varUserIdInTest = dfTest$USER_ID
dfTrain$USER_ID = NULL
dfTrain$Party = NULL
dfTest$USER_ID = NULL
dfMerged = rbind(dfTrainClean1, dfTest)
library(mice)
imputed = complete(mice(dfMerged, method = "pmm"))
dfTrainImputed = dfMerged[1:nrow(dfTrainClean1), ]
dfTestImputed = dfMerged[(nrow(dfTrainClean1)+1):nrow(dfMerged), ]
