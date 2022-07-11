#import
getwd()
PATH <- "Mall_Customers.csv"
x <- read.csv(PATH)

#Extract features
df = x[ ,c('Annual_Income','Spending_Score')]
df

#estimation
summary(df)

#pre-processing
library(dplyr)
rescale_df <- x %>%
  mutate(income_scal = scale(Annual_Income),
         spending_score_scal = scale(Spending_Score)) %>%
   select(-c(CustomerID, Gender, Age, Annual_Income, Spending_Score))

#by normalization 
norm_data=data.frame(df$Annual_Income,df$Spending_Score)
norm_data

normalize<-function(norm_data)
{
  n=(norm_data-min(norm_data))/(max(norm_data)-min(norm_data))
  n
}
result<-normalize(norm_data)
result

#training
kmeans(result, 2)
set.seed(2345)
library(animation)
kmeans.ani(result, 2)

cluster <-kmeans(result, 3)
cluster$cluster
cluster$centers
cluster$totss
cluster$withinss
cluster$tot.withinss
cluster$betweenss
cluster$size

kmean_withinss <- function(k) {
  cluster <- kmeans(result, k)
  return (cluster$tot.withinss)
}
kmean_withinss(2)
max_k <-20
wss <- sapply(2:max_k, kmean_withinss)
wss

elbow <-data.frame(2:max_k, wss)
library(ggplot2)
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))


cluster2 <-kmeans(result, 5)

#centers of clusters
cluster2$centers

#error function 
#ecu <- sqrt(cluster2$totss)
#ecu

#plotting clusters
kmeans(result, 5)
set.seed(2345)
library(animation)
kmeans.ani(result, 5)
