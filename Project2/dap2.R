#prop.table(table(default))
#prop.table(table(sex))

df$marriage <- as.factor(marriage)
df$sex <- as.factor(sex)
df$age <- as.integer(as.factor(age))
#df$pay_0 <- as.factor(pay_0)

# Is sex an accurate predictor of credit card default?
glm.fit = glm(default ~ sex, data = df, family = "binomial")
#summary(glm.fit)
glm.probs = predict(glm.fit, type = "response")
#glm.probs[1:5]
glm.pred = ifelse(glm.probs < 0.2212, 1, 0)
#table(glm.pred, default)

#p2 = mosaic(default ~ sex, data = df)

# K-Nearest Neighbors

vars <- c("pay_0", "bill_amt1", "pay_amt1")
df.subset <- df[vars]

test <- 1:9000
df.train <- df.subset[-test,]
df.test <- df.subset[test,]

train.def <- df$default[-test]
test.def <- df$default[test]

knn.1 <-  knn(df.train, df.test, train.def, k=1)
knn.5 <-  knn(df.train, df.test, train.def, k=5)
knn.20 <- knn(df.train, df.test, train.def, k=20)
knn.100 <- knn(df.train, df.test, train.def, k=100)

#100 * sum(test.def == knn.20)/9000

#prop.table(table(knn.1, test.def),1)
#prop.table(table(knn.5, test.def),1)
#prop.table(table(knn.20, test.def),2)
#table(knn.20, test.def)

#prop.table(table(knn.1, test.def),2)
#table(knn.1, test.def)
#table(test.def)

p3 = ggplot(df, aes(x = pay_0, y = bill_amt1, color = default)) + geom_jitter()
