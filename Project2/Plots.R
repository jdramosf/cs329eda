require(MASS)
require(ISLR)
require(ggplot2)
require(data.world)


summary(df)
attach(df)

#print(df)

#LDA w payment timings

df$default <- as.integer(as.logical(df$default))

lda.fit=lda(default~pay_0+pay_2+pay_3, data=df)

lda.fit
#p1 <- plot(lda.fit)
lda.pred=predict(lda.fit)
class(lda.pred)
data.frame(lda.pred)[1:50,]
df1 = data.frame(lda.pred)
head(df)

#p1 <- ggplot(df1) + geom_histogram(mapping = aes(x=df$LD1)) + facet_wrap(~ class)

table(lda.pred$class,df$default)
mean(lda.pred$class==df$default)
#p1 <- ggplot(df, aes(x = df$LD1)) + geom_histogram()


p1 = ggplot(data=df, aes(default)) + geom_bar()

# QDA

qda.fit=qda(default~pay_0+pay_2+pay_3, data=df)

qda.fit
#plot(qda.fit)
qda.pred=predict(qda.fit)
class(qda.pred)
data.frame(qda.pred)[1:3,]
class(qda.pred)
df1 = data.frame(qda.pred)
head(df1)

table(qda.pred$class,df$default)
mean(qda.pred$class==df$default)