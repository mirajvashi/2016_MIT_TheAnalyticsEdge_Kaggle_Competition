SOURCE: https://inclass.kaggle.com/c/can-we-predict-voting-outcomes/forums/t/21661/score-of-0-674

Dave, @hellerox. My code is a little disorganized, but here is how I did cross validation.

crossValidate <- function(df, nfolds, modeler,alpha=0) {
  cv.acc <- vector(mode="numeric", length=nfolds)
  set.seed(113341)
  folds <- sample(rep(1:nfolds,length=nrow(df)))
  for(k in 1:nfolds) {
    pred <- modeler(train=df[folds!=k,],test=df[folds==k,],alpha=alpha)
    tab <- table(df[folds == k,]$Party,pred>0.5)
    cv.acc[k] <- sum(diag(tab))/sum(tab)
    print(paste0("Finished fold ",k,"/",nfolds))
  } 
  avgAcc <- mean(cv.acc)
  return (avgAcc)
}
And this is an example of a "modeler"

rfModeler <- function(train,test,alpha=2601) {
  print(paste0("Running rfModeler"))
  rfMod6 <- randomForest(Party ~ . -USER_ID-submit, data=train, ntree=alpha)
  rfTestPred <- predict(rfMod6, newdata=test,type="prob")[,2]
  return (rfTestPred)
}
And used this way to get cross validated accuracy:

rfTestAccuracy1 <- crossValidate(df=train,nfolds=5,alpha=2601,modeler=rfModeler)