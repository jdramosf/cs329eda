require(MASS)
require(ISLR)
require(ggplot2)
require(data.world)
require(plotly)
require(dplyr)
require(glmnet)
require(e1071)
require

project <- "https://data.world/ananya-kaushik/f-17-eda-project-5"
data.world::set_config(cfg_env("DW_API"))
abalone <- data.world::query(
  data.world::qry_sql("select * from abalone_data"),
  dataset = project
)
attach(abalone)
summary(abalone)
colnames(abalone) <- c("sex", "length", "diameter", "height", "wholeweight", "shuckedweight", "visceraweight", "shellweight", "rings")
abalone$sex = as.factor(abalone$sex)
attach(abalone)


#Exploration: removing outliers
abalone2 <- abalone%>%dplyr::filter(height<=.3)
ggplot(abalone2, aes(x=rings,y=height))+geom_jitter()
#plot_ly(data=abalone, x = ~rings, y = ~height)


#Ridge Regression
x=model.matrix(rings~.-1,data=abalone) #construct a matrix of the x'es
y=abalone$rings

fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
labs = names(abalone)
#legend("topright", legend = labs, lty=1, col=1:10, cex = 0.6)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)


#Lasso
fit.lasso=glmnet(x,y) #x is the model matrix. lasso - the coefficient goes to 0, depending on lambda
plot(fit.lasso,xvar="lambda",label=TRUE) #plot how lambda changes with the coffecients. coefficients are shrinking with with increase in log lambda.
cv.lasso=cv.glmnet(x,y) #cross validation. default  is 100 lambdas, so 100 models
plot(cv.lasso) #mean squared error of all 100 models
coef(cv.lasso)


#Support Vector Machines
ringbuckets2 = cut(abalone$rings, c(1, 10, 30), right = FALSE, labels = c("< 10", "> 10"))
#attach(abalone)
df = data.frame(output = as.factor(ringbuckets2), shellweight = abalone$shellweight, length = abalone$length)
plot_ly(data = df, x=~length, y=~shellweight)
svmfit=svm(output~.,df,kernel="linear",cost=10,scale=FALSE)
print(svmfit)
plot(svmfit, df, shellweight ~ length)
# tune.out <- tune(svm, output ~., data=df, kernel='linear',
#                  ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100,1000)))
# summary(tune.out)

# svmfit2=svm(output~.,df,kernel="polynomial",cost=10,scale=FALSE)
# print(svmfit2)
# plot(svmfit2, df, shellweight ~ length)

svmfit3=svm(output~.,df,kernel="radial",cost=10,scale=FALSE)
print(svmfit3)
plot(svmfit3, df, shellweight ~ length)

