########################
### Cluster based CF ###
########################  

### Read In data ###
####################
#install.packages("recommenderlab")
library(recommenderlab)

UserArtists <- read.csv("C:/Users/ckadic/Desktop/RecommendationTools/GroupAssignmentData/user_artists.dat", header=TRUE, sep="\t")
UserArtistsMatrix <- as(UserArtists,"matrix")


### Clustering based on the original matrix ###
###############################################
UserArtistsMatrix2 <- UserArtistsMatrix

# fill with average product rating
colmeans <- colMeans(UserArtistsMatrix2, na.rm=TRUE)

for (j in colnames(UserArtistsMatrix2)){
  UserArtistsMatrix2[, j] <- ifelse(is.na(UserArtistsMatrix2[ ,j]), colmeans[j], UserArtistsMatrix2[, j])
}

km <- kmeans(UserArtistsMatrix2, 200, iter.max=100)

head(km$cluster)
head(km$centers)


# Statistics of the groups
tab <- table(km$cluster)

min(tab)
max(tab)
mean(tab)

# Assign users to groups
JM <- cbind(UserArtistsMatrix, as.data.frame(km$cluster))

head(JM["km$cluster"])

# Calculate average ratings for everi cluster
aggregation <- aggregate(JM, list(JM$"km$cluster"), mean, na.rm=T)
colnames(aggregation)
aggregation <- aggregation[,-1]
colnames(aggregation)

# Make a prediction
users <- as.data.frame(JM$"km$cluster")
users <- cbind(users, rownames(JM))
colnames(users) <- c("km$cluster", 'user')

prediction = merge(users, aggregation, by="km$cluster")
rownames(prediction) <- prediction$user

prediction  <- prediction[order(rownames(prediction)), -1:-2]

# TopN
TopN <- t(apply(prediction, 1, function(x) names(head(sort(x, decreasing=TRUE), 5))))

##############################################################################################
### Clustering based on the distance matrix ###
###############################################
install.packages("proxy")
library("proxy")
d <- dist(as.matrix(UserArtistsMatrix), method = "cosine")

hc <- hclust(d)
plot(hc)

# Create groups
grouping <- cutree(hc, k=200)

# Statistics of the groups
tab <- table(grouping)

min(tab)
max(tab)
mean(tab)

# Assign users to groups
JM <- cbind(UserArtistsMatrix, as.data.frame(grouping))

# Calculate average ratings for everi cluster
aggregation <- aggregate(JM, list(JM$grouping), mean, na.rm=T)
aggregation <- aggregation[,-1]

# Make a prediction
users <- as.data.frame(JM$grouping)
users <- cbind(users, rownames(JM))
colnames(users) <- c("grouping", 'user')

predictionDist = merge(users, aggregation, by="grouping")
rownames(predictionDist) <- predictionDist$user

predictionDist  <- predictionDist[, -1:-2]

# TopN
TopNDist <- t(apply(predictionDist, 1, function(x) names(head(sort(x, decreasing=TRUE), 5))))


########################################################################################################################################
### Cluster based CF as a function ###
######################################

ClusterBasedCF <- function(data, N, centers, iter, onlyNew=TRUE){
  
  data2 <- data
  
  # fill with average product rating
  colmeans <- colMeans(data2, na.rm=TRUE)
  
  for (j in colnames(data2)){
    data2[, j] <- ifelse(is.na(data2[ ,j]), colmeans[j], data2[, j])
  }
  
  km <- kmeans(data2, centers=centers, iter.max=iter)
  
  head(km$cluster)
  head(km$centers)
  
  
  # Statistics of the groups
  tab <- table(km$cluster)
  
  # Assign users to groups
  RES <- cbind(data, as.data.frame(km$cluster))
  
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
      unseen <- names(data[u, is.na(data[u,])])
      
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