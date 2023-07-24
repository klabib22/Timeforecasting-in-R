library(ggplot2)

install.packages("factoextra")
library(factoextra)

df <-read.csv("C:\\Users\\Hp\\Downloads\\winequality-red.csv")
df

str(df)

df <- scale(df)
df

cluster <- kmeans(df, centers = 5, nstart =30)
fviz_cluster(cluster, data = df)

#changing no of clusters
c <- kmeans(df, centers = 3, nstart = 20)
fviz_cluster(c,data = df)

install.packages("fpc")
library(fpc)

dbscan1 <- dbscan(df, eps = 0.25, MinPts = 2)
dbscan1
dbscan1$cluster
plot(dbscan1,df,main = "DBSCAN clustering for wine")
