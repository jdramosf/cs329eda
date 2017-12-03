library(tidyverse)
library(class)
require(data.world)
require(ggplot2)
require(shiny)
require(dplyr)
require(ISLR)
require(vcd)
require(plotly)
require(boot)
require(leaps)
require(tree)
require(randomForest)
require(gbm)

project <- "https://data.world/ananya-kaushik/f-17-eda-project-5"

abalone <- data.world::query(
  data.world::qry_sql("select * from abalone_data"),
  dataset = project
)

colnames(abalone) <- c("sex", "length", "diameter", "height", "wholeweight", "shuckedweight", "visceraweight", "shellweight", "rings")
abalone$sex = as.factor(abalone$sex)
summary(abalone)
attach(abalone)

# Let's see if we can identify strong relationships between the number of rings found in an abalone and the rest of features collected on this dataset
pairs(abalone[2:9])
# We exclude sex because is a qualitative variable
# We observe that the number of rings has a positive relationship with most of the rest of the variables included in the visualization
# We will further explode whether these relationships are significant in determining the number of rings in an abalone and, consequently, their age.

# Multi predictor linear regression
loocv = function(fit) {
  h = lm.influence(fit)$h
  mean((residuals(fit) / (1 - h)) ^ 2)
}
multiabalone = lm(rings ~ ., data = abalone)
summary(multiabalone)
loocv(multiabalone)

# Forward stepwise regression using cross validation

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[,names(coefi)] %*% coefi
}

set.seed(11)
folds = sample(rep(1:10, length = nrow(abalone)))
cv.errors = matrix(NA, 10, 9)
for (k in 1:10) {
  best.fit = regsubsets(rings ~ ., data = abalone[folds != k,], nvmax = 9, method = "forward")
  for (i in 1:9) {
    pred = predict(best.fit, abalone[folds == k,], id = i)
    cv.errors[k, i] = mean((abalone$rings[folds == k] - pred) ^ 2)
  }
}

rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b")

# Bagging, Random Forest, and Boosting
set.seed(1011)
train = sample(1:nrow(abalone), 2500)
rf.abalone = randomForest(rings ~ ., data = abalone, subset = train)
rf.abalone

oob.err = double(8)
test.err = double(8)
for (mtry in 1:9) {
  fit = randomForest(rings ~ ., data = abalone, subset = train, mtry = mtry, ntree = 500)
  oob.err[mtry] = fit$mse[500]
  pred = predict(fit, abalone[-train,])
  test.err[mtry] = with(abalone[-train,], mean((rings - pred) ^ 2))
  cat(mtry, " ")
}

matplot(1:mtry, cbind(test.err, oob.err), pch = 19, col = c("red", "blue"), type = "b", ylab = "Mean Squared Error")
legend("right", legend = c("OOB", "Test"), pch = 19, col = c("red", "blue"))

boost.abalone = gbm(rings ~ ., data = abalone[train,], distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.abalone)
plot(boost.abalone, i = "shellweight")
plot(boost.abalone, i = "shuckedweight")

n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.abalone, newdata = abalone[-train,], n.trees = n.trees)
dim(predmat)
berr = with(abalone[-train,], apply((predmat - rings) ^ 2, 2, mean))
plot(n.trees, berr, pch = 19, ylab = "Mean Squared Error", xlab = "# Trees", main = "Boosting Test Error")
points(x = 2000, y = min(berr), pch = 19, col = "red")

