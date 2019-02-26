########################
### Cluster based CF ###
########################  

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
UA <- cbind(UserArtistsMatrix, as.data.frame(km$cluster))

head(UA["km$cluster"])

# Calculate average ratings for everi cluster
aggregation <- aggregate(UA, list(UA$"km$cluster"), mean, na.rm=T)
colnames(aggregation)
aggregation <- aggregation[,-1]
colnames(aggregation)

# Make a prediction
users <- as.data.frame(UA$"km$cluster")
users <- cbind(users, rownames(UA))
colnames(users) <- c("km$cluster", 'user')

prediction = merge(users, aggregation, by="km$cluster")
rownames(prediction) <- prediction$user

prediction  <- prediction[order(rownames(prediction)), -1:-2]

# TopN
TopN <- t(apply(prediction, 1, function(x) names(head(sort(x, decreasing=TRUE), 5))))

##############################################################################################
### Clustering based on the distance matrix ###
###############################################

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
UA <- cbind(UserArtistsMatrix, as.data.frame(grouping))

# Calculate average ratings for everi cluster
aggregation <- aggregate(UA, list(UA$grouping), mean, na.rm=T)
aggregation <- aggregation[,-1]

# Make a prediction
users <- as.data.frame(UA$grouping)
users <- cbind(users, rownames(UA))
colnames(users) <- c("grouping", 'user')

predictionDist = merge(users, aggregation, by="grouping")
rownames(predictionDist) <- predictionDist$user

predictionDist  <- predictionDist[, -1:-2]

# TopN
TopNDist <- t(apply(predictionDist, 1, function(x) names(head(sort(x, decreasing=TRUE), 5))))