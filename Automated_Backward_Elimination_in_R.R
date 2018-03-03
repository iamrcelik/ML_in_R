backwardElimination <- function(dataset1, SL) {
  numVars = length(dataset1)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = dataset1)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > SL){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      dataset1 = dataset1[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset1 = dataset[, c(1,2,3,4,5)]
backwardElimination(dataset1, SL)