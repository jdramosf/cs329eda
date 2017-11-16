require(MASS)
require(ggplot2)
require(data.world)
require(plotly)

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om5hdGFzaGFwaXJhbmk5NiIsImlzcyI6ImFnZW50Om5hdGFzaGFwaXJhbmk5Njo6YTU4NWU5NmItYWM4ZC00NDAzLWE0MjUtYzc2MDM2ZTY5MjczIiwiaWF0IjoxNTA1MzM2NTM2LCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX2FkbWluIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.HRx49T2J1V4cK7rYB7dztiIhYBR3jIGp9BCXi2DNVGdHTv-iRoJekXMeBHzsyenvzdXV4NX_X6QPe5MzIeL7Mg"))


project <- "https://data.world/natashapirani96/f-17-eda-project-4"
bank <- data.world::query(
  data.world::qry_sql("SELECT * FROM bank_additional"),
  dataset = project
)
summary(bank)
attach(bank)

# Exploratory Insight

plot_ly(x=~education, y=~default, type='histogram')




# Principal Components Analysis


nums <- sapply(df, is.numeric)
df1 <- df[ , nums]

print(df1)

princomp(df1)

bank.PCA <- princomp(df1)

summary(bank.PCA)

bank.PCA$loadings

biplot(bankPCA, scale=1)

#K Means Clustering

ggplot(bank, aes(x=age, y=cons_price_idx, color= job)) + geom_point()

ggplot(bank, aes(x=age, y=cons_price_idx, color= cons_conf_idx)) + geom_point()

set.seed(101)
bankCluster <- kmeans(df1, 3, nstart=20)
bankCluster
