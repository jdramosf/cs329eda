require(dplyr)

require(data.world)
library(tidyverse)
require(ggplot2)
require(plotly)
library(MASS)
library(ISLR)


project <- "https://data.world/jdramosf/f-17-eda-project-3"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM  cancer_reg"),
  dataset = project
)


summary(df)
attach(df)

# Scatterplots - Cancer Mortality and Coverage

plot_ly(x=~pctprivatecoverage, y=~target_deathrate, type='scatter')

plot_ly(x=~pctpubliccoverage, y=~target_deathrate, type='scatter')

# Bucketing Median Income into <50K and >50K

bucket_income <-  transform(df, group=cut(medincome, breaks=c(-Inf, 50000, Inf), labels=c("Less than 50000", "Greater than 50000")))
summary(bucket_income)
class(bucket_income$group)

# Linear Discriminant Analysis - Private Coverage as a Predictor of Median Income

lda.fit=lda(bucket_income$group~pctprivatecoverage, data=df)

lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit)

data.frame(lda.pred)[1:50,]
df1 = data.frame(lda.pred)

ggplot(df1) + geom_histogram(mapping = aes(x=LD1)) + facet_wrap(~ class)

table(lda.pred$class,bucket_income$group)
mean(lda.pred$class==bucket_income$group)

# Quadratic Discriminant Analysis

qda.fit=qda(bucket_income$group~pctmarriedhouseholds, data=df)

qda.fit
qda.pred=predict(qda.fit)

data.frame(qda.pred)[1:50,]

ggplot(data=df, aes(bucket_income$group)) + geom_bar()


table(qda.pred$class,bucket_income$group)
mean(qda.pred$class==bucket_income$group)

# K-Nearest Neighbors

# Choosing the relevant variables and creating a subset
vars <- c("pctprivatecoverage", "pctmarriedhouseholds", "avghouseholdsize")
df.subset <- df[vars]

# Creating test and train subsets
test <- 1:2000
df.train <- df.subset[-test,]
df.test <- df.subset[test,]

train.def <- bucket_income$group[-test]
test.def <- bucket_income$group[test]
length(df)
# Fitting the models
knn.1 <-  knn(df.train, df.test, train.def, k=1)
knn.5 <-  knn(df.train, df.test, train.def, k=5)
knn.20 <- knn(df.train, df.test, train.def, k=20)
knn.100 <- knn(df.train, df.test, train.def, k=100)

table(knn.1,test.def)
mean(knn.1==test.def)

table(knn.5,test.def)
mean(knn.5==test.def)

table(knn.100,test.def)
mean(knn.100==test.def)


