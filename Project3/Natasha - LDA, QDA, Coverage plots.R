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
