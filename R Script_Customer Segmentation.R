marketing = read.csv("marketing_campaign.csv", sep = "\t")
marketing
ind = c(1,3,4,6,7,8,21:29)
marketing = marketing[,-ind]
library (ggplot2)
library(FactoMineR)
library("tidyr")
library(corrplot)
library(RColorBrewer)
library(cluster)
library(factoextra)
library(tidyverse)
library(dplyr)
library(readr)
library(caret)
marketing$Age <- 2014 - marketing$Year_Birth
data <- marketing[c(-1)]
summary(data)

## removes the missing income rows
data = data[!is.na(data$Income),]
data.scaled = data
###Check Missing Values 
sapply(data.scaled, function(x) sum(is.na(x)))
##Income has 24 missing values 

###
###Histogram Plot of attributes
install.packages("tidyr")

data_long <- data %>% 
  pivot_longer(colnames(data)) %>%
  as.data.frame()
head(data_long)
####
ggp1 <- ggplot(data_long, aes(x=value)) + 
  geom_histogram()+
  facet_wrap(~name, scales = "free")
ggp1

###
##Box Plot 
library (reshape)
meltData <- melt(data.scaled)
p <- ggplot(meltData, aes(factor(variable), value))
p + geom_boxplot() + facet_wrap(~variable, scale="free")


#Check and Identify Outliers
is_outlier <- function(x){
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | 
           x > quantile(x, 0.75) + 1.5 * IQR(x))
}

outlier <- data.frame(variable = character(), 
                      sum_outliers = integer(),
                      stringsAsFactors=FALSE)

for (j in 1:(length(data.scaled)-1)){
  variable <- colnames(data.scaled[j])
  for (i in data.scaled[j]){
    sum_outliers <- sum(is_outlier(i))
  }
  row <- data.frame(variable,sum_outliers)
  outlier <- rbind(outlier, row)
}

# Identify the percentage of Outliers
for (i in 1:nrow(outlier)){
  if (outlier[i,2]/nrow(data.scaled) * 100 >= 5){
    print(paste(outlier[i,1], 
                '=', 
                round(outlier[i,2]/nrow(data.scaled) * 100, digits = 2),
                '%'))
  }
}
#Imputing outlier values
for (i in 1:14){
  for (j in 1:nrow(data.scaled)){
    if (data.scaled[[j, i]] > as.numeric(quantile(data.scaled[[i]], 0.75) + 
                                      1.5 * IQR(data.scaled[[i]]))){
      if (i == 4){
        data.scaled[[j, i]] <- round(mean(data.scaled[[i]]), digits = 2)
        data.scaled[[j, i]] <- round(mean(data.scaled[[i]]), digits = 3)
      }
    }
  }
}



#Correlation Map

M <-cor(data)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
corrplot(M, type="upper", order="hclust")

##Function for Capping
cap <- function(x){
  quantiles <- quantile(x, c(0.05, 0.25, 0.75, 0.95))
  x[x < quantiles[2] - 1.5*IQR(x)] <- quantiles[1]
  x[x < quantiles[3] + 1.5*IQR(x)] <- quantiles[4]
x}
#####

##Apply function cap to data frame
data_cap <- sapply(data.scaled, FUN = cap)

###Check distribution of attributes after capping 
###Replacing the outliers with the nearest neighbours that are not outliers. For example, for outliers 
##that lie outside the outlier fences on a box plot, we can cap it by replacing those observations outside 
##the lower limit with the value of the 5th percentile and those that lie above the upper limit of the 95th perentile

###Histogram Plot of attributes
install.packages("tidyr")

data_long1 <- data %>% 
  pivot_longer(colnames(data_cap)) %>%
  as.data.frame()
head(data_long1)
####
ggp2 <- ggplot(data_long1, aes(x=value)) + 
  geom_histogram()+
  facet_wrap(~name, scales = "free")
ggp
###################

## scaling the columns based on standardization
## standardization is where we subtract by the mean and divide by the standard
## deviation
for (col in colnames(data.scaled)){
  x = data.scaled[,col]
  avg = mean(x)
  sd = sd(x)
  data.scaled[,col] = (x - avg)/sd
}
library(FactoMineR)
library(factoextra)
library(animation)

##PCA
pca_try <- PCA(data.scaled, 
               scale.unit = FALSE,
               graph = F, 
               ncp = 10) #default: 5)
summary(pca_try)
##
plot.PCA(pca_try, 
         choix = c("ind"),
         habillage = 1,
         select = "contrib5",
         invisible = "quali")
###Outliers+
data.scaled[c(1246, 1847, 1043, 1654, 56),"tempo"]

plot.PCA(pca_try, choic = c("var"))

pca_dimdesc <- dimdesc(pca_try)
pca_dimdesc$Dim.1

##Kmeans 
d_new <- data.scaled[-c(1246, 1847, 1043, 1654, 56),]
str(d_new)
d_scale <- scale(d_new)

##
RNGkind(sample.kind = "Rounding") #to get the set.seed numbers not changed everytime executed
kmeansTunning <- function(data, maxK) {
  withinall <- NULL
  total_k <- NULL
  for (i in 2:maxK) {
    set.seed(101)
    temp <- kmeans(data,i)$tot.withinss
    withinall <- append(withinall, temp)
    total_k <- append(total_k,i)
  }
  plot(x = total_k, y = withinall, type = "o", xlab = "Number of Cluster", ylab = "Total within")
}

# kmeansTunning(your_data, maxK = 5)
kmeansTunning(d_scale, maxK = 15)

###
RNGkind(sample.kind = "Rounding")
set.seed(101)
pc_cluster <- kmeans(d_scale, centers = 2)
fviz_cluster(pc_cluster, data = d_scale)

##
d_new$cluster <- pc_cluster$cluster
d_new

##
round(prop.table(table(d_new$cluster)),2)

##
d_new %>% 
  group_by(cluster) %>% 
  summarise_all("mean")

## selecting optimal number of clusters
par(mfrow=c(1,2))
fviz_nbclust(data.scaled,kmeans,method="silhouette",k.max=10,n=5)
fviz_nbclust(data.scaled,kmeans,method="wss",k.max=10,n=5)

set.seed(425)

k = kmeans(x=data.scaled,centers=2,n=10)

k$centers

## check accuracy 
R.squared = 1 - k$tot.withinss/k$totss
R.squared

## cluster visualization
clusplot(data.scaled,k$cluster,color=T)
fviz_cluster(k, data = data,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

###
## outlier detection for later cleaning/refining of dataset
distances <- sqrt(rowSums((data.scaled - k$centers)^2))
outliers <- order(distances, decreasing=T)[1:5]
print(data.scaled[outliers,])
print(data[outliers,])



## Silhouette plot to confirm optimal clustering
windows()
d = dist(data.scaled)
plot(silhouette(k$cluster,d))

mean(silhouette(k$cluster,d)[,3])

data$cluster = k$cluster

clust1 = data[data$cluster==1,]
clust2 = data[data$cluster==2,]


## Plot average and total values for each cluster
par(mfrow=c(3,5))

for (colname in colnames(data)){
  print(colname)
  colaverages = c(mean(clust1[,colname]),mean(clust2[,colname]))
  xx = barplot(colaverages,names.arg=c("1","2"), col=c("light blue","red"),main = colname)
  text(x = xx, y = round(colaverages), label = round(colaverages), pos = 1, cex = 0.8, col = "black")
}

for (colname in colnames(data)){
  print(colname)
  colsums = c(sum(clust1[,colname]),sum(clust2[,colname]))
  xx = barplot(colsums,names.arg=c("1","2"), col=c("blue","magenta"),main = colname)
  text(x = xx, y = round(colsums), label = round(colsums), pos = 3, cex = 0.8, col = "black")
}



## Sales by Product Category
par(mfrow=c(1,2))
pie(colMeans(clust1[,c(4:9)]), col = rainbow(length(4:9)), main="cluster 1")
pie(colMeans(clust2[,c(4:9)]), col = rainbow(length(4:9)), main="cluster 2")

## Purchasing Channels
pie(colMeans(clust1[,c(11:13)]),col=rainbow(3),main="cluster 1")
pie(colMeans(clust2[,c(11:13)]),col=rainbow(3),main="cluster 2")

#### PCA

res.pca <- prcomp(data.scaled, scale = FALSE)
fviz_eig(res.pca)
res.pca$rotation

##Compute Standard deviation
res.pca$sdev
##Compute variance
pca_var <- res.pca$sdev ^ 2
pca_var

##proportion of variance for a scree plot
propve <- pca_var/ sum(pca_var)
propve

##Plot variance explained for each principal component
plot(propve, xlab = "Principal component",
     ylab = "Proportion of Variance Explained", 
     ylim = c(0,1), type = "b",
     main = "Scree plot")

###Plot of Cumulative proportion of variance explained
plot(cumsum(propve),
     xlab = "Principal Component",
     ylab = "Cumulative proportion of variance explained", 
     ylim = c(0,1), type = "b")

##Find Top n Principal Component
## which will atleast cover 90% of variance of dimensions
which(cumsum(propve) >= 0.9)[1]

###Graph of individual. Individuals with a similar profile are grouped together
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE     # Avoid text overlapping
             )
#Graph of variables . Positive correlated variables point to the same side of the plot. 
#Negative Correlated variables point to opposite side of the graph
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

##Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
princomp(marketing.scaled, scores = TRUE)
library(factoextra)
###
eig.val <- get_eigenvalue(res.pca)
eig.val

##Result for Variables 
res.var <- get_pca_var(res.pca)
res.var$coord
res.var$contrib
res.var$cos2


##Saving the PCA output as a data frame 
t_data <- as.data.frame(-res.pca$x[,1:8])
plot(res.pca$ind$coord[,1:2], col=factor(kc$cluster))

## selecting optimal number of clusters
par(mfrow=c(1,2))
fviz_nbclust(t_data, kmeans,method="silhouette",k.max=10,n=5)
fviz_nbclust(t_data,kmeans,method="wss",k.max=10,n=5)

set.seed(425)

tr = kmeans(x=t_data,centers=2,n=10)

tr$centers

## check accuracy 
R.squared_PCA = 1 - tr$tot.withinss/k$totss
R.squared_PCA

## cluster visualization
clusplot(t_data,tr$cluster,color=T)
fviz_cluster(tr, data = t_data,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


## Silhouette plot to confirm optimal clustering
windows()
dt = dist(t_data)
plot(silhouette(tr$cluster,dt))

mean(silhouette(tr$cluster,dt)[,3])

t_data$cluster = tr$cluster

clust1 = data[t_data$cluster==1,]
clust2 = data[t_data$cluster==2,]

## Plot average and total values for each cluster
par(mfrow=c(3,5))

for (colname in colnames(t_data)){
  print(colname)
  colaverages = c(mean(clust1[,colname]),mean(clust2[,colname]))
  xx = barplot(colaverages,names.arg=c("1","2"), col=c("light blue","red"),main = colname)
  text(x = xx, y = round(colaverages), label = round(colaverages), pos = 1, cex = 0.8, col = "black")
}

for (colname in colnames(data)){
  print(colname)
  colsums = c(sum(clust1[,colname]),sum(clust2[,colname]))
  xx = barplot(colsums,names.arg=c("1","2"), col=c("blue","magenta"),main = colname)
  text(x = xx, y = round(colsums), label = round(colsums), pos = 3, cex = 0.8, col = "black")
}
