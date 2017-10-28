require(MASS)
require(ISLR)
require(ggplot2)
require(data.world)
require(plotly)
require(leaps)
project <- "https://data.world/jdramosf/f-17-eda-project-3"

cancer <- data.world::query(
  data.world::qry_sql("select * from cancer_reg"),
  dataset = project
)

c2 = cancer %>% dplyr::select(medincome, povertypercent, pctnohs18_24, pcths18_24, pctsomecol18_24, pcths25_over, pctbachdeg25_over, pctmarriedhouseholds)
pairs(c2)

plot_ly(data=cancer, x = ~medincome, y = ~pctbachdeg25_over)

cancer$medincomebuckets = cut(cancer$medincome, c(0, 50000, 150000), right=FALSE, labels=c("<= 50,000", "> 50,000"))

#25+ with a bachelors degree
glm.fit1 = glm(medincomebuckets ~ pctbachdeg25_over, data=cancer, family = "binomial")
glm.fit1
summary(glm.fit1)

#predictions
glm.probs1=predict(glm.fit1,type="response") 
glm.probs1[1:5]
glm.pred1=ifelse(glm.probs1>0.5,"> 50,000","<= 50,000")
table(glm.pred1,medincomebuckets)
mean(glm.pred1==medincomebuckets)

#25+ with a bachelors degree, high school
glm.fit2 = glm(medincomebuckets ~ pctbachdeg25_over+ pcths25_over, data=cancer, family = "binomial")
glm.fit2
summary(glm.fit2)

#predictions
glm.probs2=predict(glm.fit2,type="response") 
glm.probs2[1:5]
glm.pred2=ifelse(glm.probs2>0.5,"> 50,000","<= 50,000")
table(glm.pred2,medincomebuckets)
mean(glm.pred2==medincomebuckets)

plot_ly(data=cancer, x = ~medincome, y = ~pctmarriedhouseholds)

#25+ with a bachelors degree, high school, married
glm.fit3 = glm(medincomebuckets ~ pctbachdeg25_over+ pcths25_over + pctmarriedhouseholds, data=cancer, family = "binomial")
glm.fit3
summary(glm.fit3)

#predictions
glm.probs3=predict(glm.fit3,type="response") 
glm.probs3[1:5]
glm.pred3=ifelse(glm.probs3>0.5,"> 50,000","<= 50,000")
table(glm.pred3,medincomebuckets)
mean(glm.pred3==medincomebuckets)


#Subset selection
# Our dataset has over 30 variables, and some of them are correlated to each other. Let's identify the useful ones only
subsetpred = cancer %>% dplyr::select(-binnedinc, -medianagefemale, -medianagemale, -geography, -pcths18_24, -pctnohs18_24, -pctsomecol18_24, -pctbachdeg18_24, -pctemployed16_over, -pctempprivcoverage, -medincome)

mincsubset = regsubsets(medincomebuckets ~ ., data = subsetpred, really.big = T, nvmax = 23)
summary(mincsubset)
minsubsetsum = summary(mincsubset)
#names(mincsubsetsum)
plot(minsubsetsum$cp,xlab="Number of Variables",ylab="Cp")
which.min(minsubsetsum$cp)
points(11,minsubsetsum$cp[11],pch=20,col="red")
plot(mincsubset, scale = "Cp")
coef(mincsubset, 23)
