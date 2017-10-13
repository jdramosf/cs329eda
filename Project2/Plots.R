#LDA w payment timings

df$default <- as.integer(as.logical(df$default))

lda.fit=lda(default~pay_0+pay_2+pay_3, data=df)
lda.pred=predict(lda.fit)
#data.frame(lda.pred)[1:50,]
df1 = data.frame(lda.pred)

lda.cm = table(df1$class,df$default)
mean(df1$class==df$default)

p1 = ggplot(data=df, aes(default)) + geom_bar()

# QDA

qda.fit=qda(default~pay_0+pay_2+pay_3, data=df)
qda.pred=predict(qda.fit)
#data.frame(qda.pred)[1:3,]
df2 = data.frame(qda.pred)

qda.cm = table(df2$class,df$default)
mean(df2$class==df$default)

