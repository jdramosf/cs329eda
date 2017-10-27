require(MASS)
require(ISLR)
require(ggplot2)
require(data.world)
require(plotly)
require(dplyr)
require(leaps)

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

#cancer$medincomebuckets = cut(cancer$medincome, c(0, 50000, 150000), right=FALSE, labels=c("<= 50,000", "> 50,000"))

# Creating a linear regression model, using medianage and adding other variables as we see fit
medagelm = lm(target_deathrate ~ medianage, data = cancer)
summary(medagelm)
# With a p-value of 0.8 for the medianage coefficient, we fail to reject the null hypothesis
# There is no relationship between median age and cancer death rate
# Let's add other variables
plot(target_deathrate ~ medincome, data = cancer)
medincomelm = lm(target_deathrate ~ medincome, data = cancer)
summary(medincomelm)
abline(medincomelm, col = "red")
# There seems to be some relationship between median income and cancer death rate, a negative relationship to be precise
# However, the R-squared value is too low (0.18), which means that our model does not fully explain the variance in the output variable
# This means that a multi-predictor linear model could be more accurate.

# Let's fit a linear model with more than one predictor
summary(cancer)
# Let's select predictors based on our own intuition. We believe that incidence rate, median income, population estimate, poverty percentage, unemployment percetange, and private coverage percentage could be the strongest predictors. Let's look at the relationships between these variables
multipred = cancer %>% dplyr::select(target_deathrate, incidencerate, medincome, popest2015, povertypercent, pctunemployed16_over, pctprivatecoverage)
pairs(multipred)
# (Some interesting relationships in this plot. We will explore some of these relationships in following insights)
multipredlm = lm(target_deathrate ~ ., data = multipred)
summary(multipredlm)
# We see some great results from this model! Aside from povertypercent, all of our predictors are statistically significant, and our R-squared has improved substantially (0.4284)
# This is a great start, but can we do better? We will look at better ways of identifying predictors for our model.

# Model Selection - Best Subset Regression
# Our dataset has over 30 variables, and some of them are correlated to each other. Let's identify the useful ones only
subsetpred = cancer %>% dplyr::select(-binnedinc, -medianagefemale, -medianagemale, -geography, -pcths18_24, -pctnohs18_24, -pctsomecol18_24, -pctbachdeg18_24, -pctemployed16_over, -pctprivatecoveragealone, -pctempprivcoverage, -pctpubliccoverage, -pctpubliccoveragealone)
subsetpredsubm = regsubsets(target_deathrate ~ ., data = subsetpred, really.big = T, nvmax = 20)
summary(subsetpredsubm)
subsetpredsum = summary(subsetpredsubm)
names(subsetpredsum)
plot(subsetpredsum$cp,xlab="Number of Variables",ylab="Cp")
which.min(subsetpredsum$cp)
points(16,subsetpredsum$cp[16],pch=20,col="red")
plot(subsetpredsubm, scale = "Cp")
coef(subsetpredsubm, 16)
