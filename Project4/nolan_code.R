require(MASS)
require(ISLR)
require(ggplot2)
require(data.world)
require(plotly)

project <- "https://data.world/natashapirani96/bank-additional"

bank <- data.world::query(
  data.world::qry_sql("select * from bank_additional"),
  dataset = project
)

summary(bank)
attach(bank)

plot_ly(
  x = ~job,
  y = ~age,
  type = "scatter"
)

plot_ly(
  x = ~duration,
  y = ~age,
)
  

plot_ly(
  x = ~campaign,
  y = ~age,
  type = "scatter"
  )
