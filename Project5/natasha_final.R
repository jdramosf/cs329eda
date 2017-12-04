require(MASS)
require(ggplot2)
require(data.world)
require(plotly)

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om5hdGFzaGFwaXJhbmk5NiIsImlzcyI6ImFnZW50Om5hdGFzaGFwaXJhbmk5Njo6YTU4NWU5NmItYWM4ZC00NDAzLWE0MjUtYzc2MDM2ZTY5MjczIiwiaWF0IjoxNTA1MzM2NTM2LCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX2FkbWluIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.HRx49T2J1V4cK7rYB7dztiIhYBR3jIGp9BCXi2DNVGdHTv-iRoJekXMeBHzsyenvzdXV4NX_X6QPe5MzIeL7Mg"))


project <- "https://data.world/ananya-kaushik/f-17-eda-project-5"
abalone <- data.world::query(
  data.world::qry_sql("SELECT * FROM abalone_data"),
  dataset = project
)

colnames(abalone) <- c("sex", "length", "diameter", "height", "wholeweight", "shuckedweight", "visceraweight", "shellweight", "rings")
summary(abalone)
attach(abalone)
names(abalone)

ringbuckets = cut(abalone$rings, c(1, 5, 10, 15, 30), right = FALSE, labels = c("1 - 5", "6 - 10", "11 - 15", "> 15"))

# Exploratory Insight

ggplot(abalone, aes(x=diameter, y=rings, color= sex)) + geom_point()

ggplot(data=abalone, aes(sex, fill=sex)) + geom_bar(colour="black")


# K-Nearest Neighbors
library(class)

ggplot(abalone, aes(x=ringbuckets)) + geom_histogram(stat="count", colour="black", fill="blue", binwidth=.05)

# Choosing the relevant variables and creating a subset
vars = cbind(diameter, wholeweight)
df.subset <- vars
df.subset

# Creating test and train subsets
test <- 1:2500
df.train <- df.subset[-test,]
df.test <- df.subset[test,]

train.def <- ringbuckets[-test]
test.def <- ringbuckets[test]


# Fitting the models
knn.1 <-  knn(df.train, df.test, train.def, k=1)
knn.5 <-  knn(df.train, df.test, train.def, k=5)
knn.20 <- knn(df.train, df.test, train.def, k=20)
knn.100 <- knn(df.train, df.test, train.def, k=100)

table(knn.1,test.def)
mean(knn.1==test.def)

table(knn.5,test.def)
mean(knn.5==test.def)

table(knn.20,test.def)
mean(knn.20==test.def)

table(knn.100,test.def)
mean(knn.100==test.def)


# K Means Clustering

ab.km = cbind(diameter, shellweight)
ab.km

set.seed(101)
abaloneCluster <- kmeans(ab.km, 4, nstart=20)
abaloneCluster

abaloneCluster$cluster <- as.factor(abaloneCluster$cluster)
ggplot(abalone, aes(x=diameter, y=shellweight, color = abaloneCluster$cluster)) + geom_point()

# Compare to ringbuckets variable

ggplot(abalone, aes(x=diameter, y=shellweight, color= ringbuckets)) + geom_point()

table(abaloneCluster$cluster, ringbuckets)

summary(ringbuckets)
summary(abaloneCluster$cluster)
# Hierarchical Clustering

ab.km.subset <- ab.km[1:400]


clusters <- hclust(dist(ab.km), method='average')
plot(clusters)

clusterCut <- cutree(clusters, 5)
clusterCut
plot(clusterCut)

# Comparison to ringbuckets variable

table(clusterCut, ringbuckets)

ggplot(abalone, aes(diameter, rings, color = ringbuckets)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green', 'blue'))



# Principal Component Analysis

ab.continuous = cbind(length, diameter, height, wholeweight, shuckedweight, visceraweight, shellweight)

ab.pca <- prcomp(ab.continuous, center = TRUE, scale. = TRUE)
print(ab.pca)

plot(ab.pca, type="l")

summary(ab.pca)

biplot(ab.pca)
