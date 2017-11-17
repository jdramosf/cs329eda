require(ISLR)
require(tree)
require(data.world)
require(plotly)
require(dplyr)


project <- "https://data.world/natashapirani96/f-17-eda-project-4"

bank <- data.world::query(
  data.world::qry_sql("select * from bank_additional"),
  dataset = project
)


bank$output = as.factor(bank$y)
na.omit(bank)
bank = bank %>% mutate_if(is.character, as.factor)
attach(bank)

plot_ly(x = ~output, type = "histogram")

tree.bank = tree(output ~ . - y, data = bank)
summary(tree.bank)
plot(tree.bank)
text(tree.bank, pretty = 0)
tree.bank



set.seed(1011)
train = sample(1:nrow(bank), 2000)
tree.bank = tree(output ~ . - y, bank, subset = train)
plot(tree.bank);text(tree.bank, pretty = 0)
tree.pred = predict(tree.bank, bank[-train,], type="class")
with(bank[-train,], table(tree.pred, output))
(1855+72)/2119


cv.bank = cv.tree(tree.bank, FUN = prune.misclass)
cv.bank
plot(cv.bank)
prune.bank = prune.misclass(tree.bank, best = 8)
plot(prune.bank);text(prune.bank, pretty = 0)


tree.pred = predict(prune.bank, bank[-train,], type="class")
with(bank[-train,], table(tree.pred, output))
(1850+88)/2119


# Random Forests
require(randomForest)
require(MASS)
set.seed(101)
dim(bank)
train = sample(1:nrow(bank), 2000)

rf.bank = randomForest(output ~ .-y, data = bank, subset = train)
rf.bank