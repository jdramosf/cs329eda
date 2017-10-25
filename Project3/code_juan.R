require(MASS)
require(ISLR)
require(ggplot2)
require(data.world)
require(plotly)

project <- "https://data.world/jdramosf/f-17-eda-project-3"

cancer <- data.world::query(
  data.world::qry_sql("select * from cancer_reg"),
  dataset = project
)

summary(cancer)
attach(cancer)

plot_ly(data = cancer, x = ~medianage, y = ~target_deathrate)
medianage_nooutliers = medianage[medianage < 100]
deathrate_nooutliers = cancer$target_deathrate[medianage < 100]
plot_ly(data = cancer, x = ~medianage_nooutliers, y = ~deathrate_nooutliers)

cancer$medincomebuckets = cut(cancer$medincome, c(0, 50000, 150000), right=FALSE, labels=c("<= 50,000", "> 50,000"))
