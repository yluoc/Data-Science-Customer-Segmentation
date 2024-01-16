customer_data=read.csv("Mall_Customers.csv")
str(customer_data)

names(customer_data)

head(customer_data)
summary(customer_data$Age)

sd(customer_data$Age)
summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)
summary(customer_data$Age)

sd(customer_data$Spending.Score..1.100.)
# customer gender visualization
a = table(customer_data$Gender)
barplot(a, main = "Barplot for gender comparision",
        ylab = "Count#",
        xlab = "Gender",
        col = rainbow(2),
        legend = rownames(a))

lbls = c("Female", "Male")
pct = round(a/sum(a)*100)

lbls = paste(lbls," ", pct, "%", sep="")
pie(a, labels = lbls, col = rainbow(length(lbls)),
    main = "piechart for gender")

# customer age visualization
summary(customer_data$Age)
hist(customer_data$Age,
     col = "green",
     main = "histogram of customer age",
     xlab = "Age range",
     ylab = "Number",
     labels = TRUE)

boxplot(customer_data$Age,
        col = "blue",
        main = "boxplot for Descriptive Analysis of Age")

# analysis of customer's annual income
summary(customer_data$Annual.Income..k..)
hist(customer_data$Annual.Income..k..,
     col = "#660033",
     main = "Histogram of annual income",
     xlab = "annual income range",
     ylab = "age range",
     labels = TRUE)

plot(density(customer_data$Annual.Income..k..),
     col = "yellow",
     main = "density plot for annual income",
     xlab = "annual income range",
     ylab = "density")
polygon(density(customer_data$Annual.Income..k..),
        col = "#ccff66")

# analysis spending score of the customers
summary(customer_data$Spending.Score..1.100.)
boxplot(customer_data$Spending.Score..1.100.,
        horizontal = TRUE,
        col = "#990000",
        main = "boxplot for descriptive analysis of spending score")

hist(customer_data$Spending.Score..1.100.,
     main = "histogram for spending score",
     xlab = "spending score range",
     ylab = "number",
     col = "purple",
     labels = TRUE)

#K-means Algo
#Elbow method
library(purrr)
set.seed(123)

iss <- function(k) {
  kmeans(customer_data[,3:5], k, iter.max = 100, nstart = 100, algorithm = "Lloyd")$tot.withinss
}
k.values <- 1:10

iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "number of clusters K",
     ylab = "total iss")

#average silhouette method
library(cluster)
library(gridExtra)
library(grid)

k2 <- kmeans(customer_data[,3:5], 2, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s2 <- plot(silhouette(k2$cluster, dist(customer_data[,3:5], "euclidean")))

k3 <- kmeans(customer_data[,3:5], 3, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s3 <- plot(silhouette(k3$cluster, dist(customer_data[,3:5], "euclidean")))

k4 <- kmeans(customer_data[,3:5], 4, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s4 <- plot(silhouette(k4$cluster, dist(customer_data[,3:5], "euclidean")))

k5 <- kmeans(customer_data[,3:5], 5, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s5 <- plot(silhouette(k5$cluster, dist(customer_data[,3:5], "euclidean")))

k6 <- kmeans(customer_data[,3:5], 6, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s6 <- plot(silhouette(k6$cluster, dist(customer_data[,3:5], "euclidean")))

k7 <- kmeans(customer_data[,3:5], 7, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s7 <- plot(silhouette(k7$cluster, dist(customer_data[,3:5], "euclidean")))

k8 <- kmeans(customer_data[,3:5], 8, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s8 <- plot(silhouette(k8$cluster, dist(customer_data[,3:5], "euclidean")))

k9 <- kmeans(customer_data[,3:5], 9, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s9 <- plot(silhouette(k9$cluster, dist(customer_data[,3:5], "euclidean")))

k10 <- kmeans(customer_data[,3:5], 10, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s10 <- plot(silhouette(k10$cluster, dist(customer_data[,3:5], "euclidean")))

library(cluster)
library(factoextra)

fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")

set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

fviz_gap_stat(stat_gap)

#Visualizing the Clustering Results using the First Two Principle Components
pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[,1:2]

set.seed(1)
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

ggplot(customer_data, aes(x =Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))
