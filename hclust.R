library(stats)  ## for dist
library(NbClust)
library(cluster)
library(mclust)
library(amap)  ## for using Kmeans (notice the cap K)
library(factoextra) ## for cluster vis, silhouette, etc.
library(purrr)
library(philentropy)  ## for distance() which offers 46 metrics
library(SnowballC)
library(caTools)
library(dplyr)
library(textstem)
library(stringr)
library(wordcloud)
library(lsa)

setwd("/Users/charan/Downloads/Temporary")
Record_3D_DF<-read.csv("numericals.csv")

head(Record_3D_DF)
str(Record_3D_DF)

## remove column 1
Record_3D_DF <- Record_3D_DF[ ,-c(1) ]
head(Record_3D_DF)

Record_3D_DF <- Record_3D_DF[sample(nrow(Record_3D_DF), size=100), ]
dim(Record_3D_DF)

## Create a normalized version of Record_3D_DF
Record_3D_DF_Norm <- as.data.frame(apply(Record_3D_DF[,1:2 ], 2, ##2 represents operations column wise
                                          function(x) (x - min(x))/(max(x)-min(x))))

CosSim <- 1 - cosine(t(as.matrix(Record_3D_DF_Norm)))

# ward.D2" = Ward's minimum variance method - however dissimilarities are **squared before clustering. 
# "single" = Nearest neighbours method. 
# "complete" = distance between two clusters is defined as the maximum distance between an observation in one.

## Now run hclust...you may use many methods - Ward, Ward.D2, complete, etc..
dis <- as.dist(CosSim)
HClust_Ward_Euc_N_3D <- hclust(dis, method = "complete" )
plot(HClust_Ward_Euc_N_3D, cex=0.9, hang=-1, main = "Cosine Similarity")
rect.hclust(HClust_Ward_Euc_N_3D, k=4)
