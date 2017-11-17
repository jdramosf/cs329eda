require(ISLR)
require(ggplot2)
require(data.world)
require(plotly)
require(dplyr)
require(tree)

project <- "https://data.world/jdramosf/f-17-eda-project-3"

cancer <- data.world::query(
  data.world::qry_sql("select * from cancer_reg"),
  dataset = project
)

summary(cancer)
attach(cancer)

subsetpred = cancer %>% dplyr::select(-binnedinc, -medianagefemale, -medianagemale, -geography, -pcths18_24, -pctnohs18_24, -pctsomecol18_24, -pctbachdeg18_24, -pctemployed16_over, -pctprivatecoveragealone, -pctempprivcoverage, -pctpubliccoverage, -pctpubliccoveragealone)
dim(subsetpred)
train = sample(1:nrow(cancer), 1500)

rf.cancer = randomForest(target_deathrate ~ ., data = subsetpred, subset = train)
rf.cancer

# Let's look at the out-of-bag error and test error
oob.err = double(20)
test.err = double(20)
for(mtry in 1:20){
  fit = randomForest(target_deathrate ~ ., data = subsetpred, subset = train, mtry = mtry, ntree=500)
  oob.err[mtry] = sqrt(fit$mse[500])
  pred = predict(fit, subsetpred[-train,])
  test.err[mtry] = with(subsetpred[-train,], sqrt(mean((target_deathrate - pred)^2)))
  cat(mtry," ")
}
matplot(1:mtry, cbind(test.err, oob.err), pch=19, col=c("red","blue"), type="b", ylab="Mean Squared Error")
legend("topright", legend=c("OOB","Test"), pch=19, col=c("red","blue"))

# Boosting
--------
  Boosting builds lots of smaller trees. Unlike random forests, each new tree in boosting tries to patch up the deficiencies of the current ensemble.
```{r}
require(gbm)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.boston)
plot(boost.boston,i="lstat")
plot(boost.boston,i="rm")
```
Lets make a prediction on the test set. With boosting, the number of trees is a tuning parameter, and if we have too many we can overfit. So we should use cross-validation to select the number of trees. We will leave this as an exercise. Instead, we will compute the test error as a function of the number of trees, and make a plot.

```{r}
n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.boston,newdata=Boston[-train,],n.trees=n.trees)
dim(predmat)
berr=with(Boston[-train,],apply( (predmat-medv)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error")
abline(h=min(test.err),col="red")
```