# Let's transform some variables first so that is easy to work with them later on
df$marriage <- as.factor(marriage)
df$sex <- as.factor(sex)
df$age <- as.integer(as.factor(age))
df$default <- as.factor(as.logical(df$default))

# Logistic Regression

# Is sex an accurate predictor of credit card default?
glm.fit = glm(default ~ sex, data = df, family = "binomial")
glm.probs = predict(glm.fit, type = "response")
glm.pred = ifelse(glm.probs < 0.2212, 1, 0)
#table(glm.pred, default)

# Can we use the timing of individuals' payments to predict their default likelihood?
df$default <- as.numeric(as.logical(df$default))

# Linear Discriminant Analysis
lda.fit=lda(default~pay_0+pay_2+pay_3, data=df)
lda.pred=predict(lda.fit)
df1 = data.frame(lda.pred)

# Quadratic Discriminant Analysis
qda.fit=qda(default~pay_0+pay_2+pay_3, data=df)
qda.pred=predict(qda.fit)
df2 = data.frame(qda.pred)

# K-Nearest Neighbors

# Choosing the relevant variables and creating a subset
vars <- c("pay_0", "bill_amt1", "pay_amt1")
df.subset <- df[vars]

# Creating test and train subsets
test <- 1:9000
df.train <- df.subset[-test,]
df.test <- df.subset[test,]

train.def <- df$default[-test]
test.def <- df$default[test]

# Fitting the models
knn.1 <-  knn(df.train, df.test, train.def, k=1)
knn.5 <-  knn(df.train, df.test, train.def, k=5)
knn.20 <- knn(df.train, df.test, train.def, k=20)
knn.100 <- knn(df.train, df.test, train.def, k=100)

