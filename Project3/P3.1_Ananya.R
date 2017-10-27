require(MASS)
require(ISLR)
require(ggplot2)
require(data.world)
require(plotly)
require(dplyr)

project <- "https://data.world/jdramosf/f-17-eda-project-3"

cancer <- data.world::query(
  data.world::qry_sql("select * from cancer_reg"),
  dataset = project
)

c1 <- cancer%>%dplyr::filter(avganncount<20000)
c2 <- cancer%>%dplyr::filter(avgdeathsperyear<5000)

c3 <- cancer%>%dplyr::filter(avganncount<5000)
c4 <- cancer%>%dplyr::filter(avgdeathsperyear<1500)

summary(cancer)
attach(cancer)
#names(cancer)

ggplot(cancer, aes(x=medincome,y=avganncount)) + geom_point(color = "green4")
ggplot(cancer, aes(x=medincome,y=avgdeathsperyear)) + geom_point(color = "red4")
ggplot(c1, aes(x=medincome,y=avganncount)) + geom_point(color = "green4")
ggplot(c2, aes(x=medincome,y=avgdeathsperyear)) + geom_point(color = "red4")

ggplot(c3, aes(x=medincome,y=avganncount)) + geom_point(color = "green4")
ggplot(c4, aes(x=medincome,y=avgdeathsperyear)) + geom_point(color = "red4")
