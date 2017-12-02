require(MASS)
require(ISLR)
require(ggplot2)
require(data.world)
require(plotly)

project <- "https://data.world/ananya-kaushik/abalone"

abalone <- data.world::query(
  data.world::qry_sql("select * from abalone_data"),
  dataset = project
)
colnames(abalone) <- c("sex", "length", "diameter", "height", "wholeweight", "shuckedweight", "visceraweight", "shellweight", "rings")

summary(abalone)
attach(abalone)

# informational insight

plot_ly(
  x = ~length,
  y = ~shellweight,
  type = "scatter"
)

catabalone <- abalone
ringbuckets = cut(abalone$rings, c(1, 5, 10, 15, 30), right = FALSE, labels = c("1 - 5", "6 - 10", "11 - 15", "> 15"))

summary(ringbuckets)

abalone$wholeweight <- as.factor(wholeweight)
catabalone$rings <- ringbuckets
attach(catabalone)


summary(as.factor(sex))


# Linear Discriminent Analysis

lda.fit = lda(rings ~ shellweight, data = catabalone)
lda.pred = predict(lda.fit)
df1 = data.frame(lda.pred)

summary(df1)

ggplot(df1) + geom_histogram(mapping = aes(x=shellweight)) + facet_wrap(~ class)

# Quadratic Discriminant Analysis

qda.fit = qda(rings ~ shuckedweight, data = catabalone)
qda.pred = predict(qda.fit)
df2 = data.frame(qda.pred)

summary(df2)

ggplot(data=df2, aes(rings)) + geom_bar()
