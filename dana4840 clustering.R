##ASSIGNMENT DANA 4840 NO-2 CLUSTERING ANALYSIS
## DUE DATE- 28 FEBRUARY 2022
## NAME- RADHIKA MAINI (100340257)
## QUESTION NO 1
## CLEANING THE DATASET-) Clean the provided dataset and remove the column "BAD", store the dataset as "DF_WITH_CAT".
hmeq <- read.csv("C:/Users/16043/Downloads/hmeq.csv")
View(hmeq)
## GETTING TO KNOW THE DATASET
str(hmeq)
dim(hmeq)
## checking the missing values
library(visdat)
vis_dat(hmeq)
vis_miss(hmeq) 
## looking at the missing values there is 6.1 % of the missing values  in the dataset so it needs to be cleaned
#check if any duplicate rows
sum(duplicated(hmeq)) 
## Deduction there are less missing values present in the dataset 
sum(is.na(hmeq))
## remove duplicate rows
distinct (hmeq)
# check NA values
sum(is.na(hmeq)) 
# remove NA
hmeq_data <-hmeq[complete.cases(hmeq),] 
sum(is.na(hmeq_data))
unique(hmeq_data$REASON)
unique(hmeq_data$JOB)
#check missing values per columns
colSums(is.na(x = hmeq_data))
#check if any duplicate rows
sum(duplicated(hmeq_data)) 
## remove bad column
df_with_cat<-hmeq_data[,-1] 
View(df_with_cat)
str(df_with_cat)
df_with_cat$REASON = as.factor(df_with_cat$REASON)
df_with_cat$JOB= as.factor(df_with_cat$JOB)
###question2
#K-mediods clustering using Gower distance 

library(cluster)
gd <- daisy(df_with_cat, metric="gower")

sw<-function(label, d)
{
  s<-silhouette(label, d)
  s<-mean(s[,3])
  return(s)
}


suggest_k_sw1<-numeric()
set.seed(123) 
for (i in 1:10)
{
  sw_temp1<-numeric()
  for (j in 2:5) 
  {
    pm<-pam(gd, j, diss = TRUE)
    sw_temp1<-c(sw_temp1, sw(pm$clustering, gd))
  }
  suggest_k_sw1<-c(suggest_k_sw1, which.max(sw_temp1)+1)
}


table(suggest_k_sw1) 


set.seed(123)
pm1<-pam(gd, 5, diss = TRUE) 
pm1$clusinfo 




par(mfrow=c(2,6)) 
df_with_cat$CLUSTER<-as.factor(pm1$clustering) 
for (i in 1:(ncol(df_with_cat)-1)) 
{
  if (is.numeric(df_with_cat[,i])) 
  {
    plot(df_with_cat$CLUSTER, df_with_cat[,i], main= colnames(df_with_cat)[i])
  }
  else 
  {
    count<-table(df_with_cat[,i],df_with_cat$CLUSTER)
    barplot(count, legend = rownames(count), main= colnames(df_with_cat)[i])
  }
}
par(mfrow=c(1,1))
#Interpretation
#clustering on DEROG and DELINQ was not good as there are many data points farther from the centriod.
ed <- dist(df_with_num)

suggest_k_sw2<-numeric()
set.seed(123) 
for (i in 1:10)
{
  sw_temp2<-numeric()
  for (j in 2:5) 
  {
    pm<-pam(ed, j, diss = TRUE)
    sw_temp2<-c(sw_temp2, sw(pm$clustering, ed))
  }
  suggest_k_sw2<-c(suggest_k_sw2, which.max(sw_temp2)+1)
}

table(suggest_k_sw2) 


set.seed(123)
pm2<-pam(ed, 2, diss = TRUE) 
pm2$clusinfo 



df_with_num <- as.data.frame(df_with_num)

par(mfrow=c(2,6)) 
df_with_num$CLUSTER<-pm2$clustering 
for (i in 1:(ncol(df_with_num)-1)) 
{
  plot(df_with_num$CLUSTER, df_with_num[,i], main= colnames(df_with_num)[i])
}
par(mfrow=c(1,1))
### question 3
df_no_cat <- select_if(df_with_cat, is.numeric)

set.seed(123)
km1 <- kmeans(scale(df_no_cat), 2)

km1$size 

install.packages('fpc')
library(fpc)
install.packages('DEoptimR')
library(DEoptimR)
pm2_CH_score <- calinhara(df_with_num, pm2$clustering)  
km1_CH_score <- calinhara(scale(df_no_cat), km1$cluster) 


pm2_sw <- silhouette(pm2$clustering, ed)
summary(pm2_sw)

##Silhouette of 3515 units in 2 clusters from silhouette.default(x = pm2$clustering, dist = ed) :
####Cluster sizes and average silhouette widths:
  ##2963       552 
##0.2630827 0.1707125 


km1_sw <- silhouette(km1$cluster, ed)
summary(km1_sw)

##Silhouette of 3515 units in 2 clusters from silhouette.default(x = km1$cluster, dist = ed) :
  ##Cluster sizes and average silhouette widths:
  ##853       2662 
##0.08050284 0.26887656 

 t1<-table(kmediod = pm2$clustering, kmeans=km1$cluster)
accuracy = sum(diag(t1))/sum(t1) 
accuracy
#0.087

