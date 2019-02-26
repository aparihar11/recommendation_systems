########################################################################################################################################
### Cluster based CF as a function ###
######################################

### Read In data ###
####################
install.packages("recommenderlab")
library(recommenderlab)
#install.packages('proxy')
library('proxy')

wd = "/Users/ckadic/Desktop/RecommendationTools/GroupAssignmentData/"
setwd(wd)
getwd()
UserArtists <- read.csv('user_artists.dat', sep="\t")
UserArtistsMatrix <- as(UserArtists,"matrix")
dim(UserArtistsMatrix)

ClusterBasedCF <- function(UserArtists, N, centers, iter, onlyNew=TRUE){
  
  UserArtists2 <- UserArtists
  
  # fill with average product rating
  colmeans <- colMeans(UserArtists2, na.rm=TRUE)
  
  for (j in colnamesUserArtists2){
    UserArtists2[, j] <- ifelse(is.na(UserArtists2[ ,j]), colmeans[j], UserArtists2[, j])
  }
  
  km <- kmeans(UserArtists2, centers=centers, iter.max=iter)
  
  head(km$cluster)
  head(km$centers)
  
  
  # Statistics of the groups
  tab <- table(km$cluster)
  
  # Assign users to groups
  RES <- cbind(UserArtists, as.data.frame(km$cluster))
  
  # Calculate average ratings for everi cluster
  aggregation <- aggregate(RES, list(RES$"km$cluster"), mean, na.rm=T)
  aggregation <- aggregation[,-1]
  
  # Make a prediction
  users <- as.data.frame(RES$"km$cluster")
  users <- cbind(users, rownames(RES))
  colnames(users) <- c("km$cluster", 'rn')
  
  
  prediction = merge(users, aggregation, by="km$cluster")
  rownames(prediction) <- prediction$rn
  
  prediction  <- prediction[order(rownames(prediction)), -1:-2]
  
  prediction2 <- matrix(, nrow=nrow(prediction), ncol(prediction), 
                        dimnames=list(rownames(prediction), colnames(prediction)))
  colnames(prediction2) <- colnames(prediction)
  rownames(prediction2) <- rownames(prediction)
  
  for (u in rownames(prediction)){
    if (onlyNew == TRUE){
      unseen <- names(UserArtists[u, is.na(UserArtists[u,])])
      
      prediction2[u, ] <- as.numeric(t(ifelse(colnames(prediction) %in% unseen, prediction[u, ], as.numeric(NA))))
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
  }
  
  # TopN
  TopN <- t(apply(prediction, 1, function(x) names(head(sort(x, decreasing=TRUE), 5))))
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
} 
