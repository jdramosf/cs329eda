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

medianage_nooutliers = medianage[medianage < 100]
pctprivcoverage_nooutliers = cancer$pctprivatecoverage[medianage < 100]

plot_ly(
  x = ~pctprivcoverage_nooutliers,
  y = ~medianage_nooutliers
)

plot_ly(
  x = ~pctmarriedhouseholds,
  y = ~target_deathrate
)
