require(MASS)
require(ISLR)
require(ggplot2)
require(data.world)
require(plotly)
require(leaps)
require(e1071)

project <- "https://data.world/natashapirani96/f-17-eda-project-4"

bank <- data.world::query(
  data.world::qry_sql("select * from bank_additional"),
  dataset = project
)
bank = na.omit(bank)
#bank$output = as.factor(bank$y)

plot_ly(data=bank, x = ~y, y = ~job, type = "scatter")
ggplot(data=bank, aes(x = job, y = loan, color = y)) + geom_jitter()
#ggplot(data=bank, aes(x = job, fill = y)) + geom_histogram()


#dat=lm(campaign ~ as.factor(job), bank) #because svm wants y to be a factor
df = data.frame(output = as.factor(bank$y), CCI = bank$cons_conf_idx, duration = bank$duration)
plot_ly(data = df, x=~duration, y=~CCI)
svmfit=svm(output~.,df,kernel="linear",cost=1000,scale=FALSE)
print(svmfit)
plot(svmfit, df, CCI ~ duration)

# nlfit = svm(y ~ duration + CCI, data=df, kernel = "radial", cost = 5, type = "C")
# print(nlfit)
# plot(nlfit, data=df, CCI ~ duration)
# points(x = df$CCI, y = df$duration)
# nlfit=svm(output~.,data=df,scale=FALSE,kernel="radial",cost=1000)
# print(nlfit)
# plot(nlfit, df, CCI ~ duration)
