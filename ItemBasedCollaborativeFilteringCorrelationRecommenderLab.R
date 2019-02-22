#######################
### User - based CF with RecommenderLab ###
#######################   
#install.packages("recommenderlab")
library(recommenderlab)

### Read In data ###
####################
recommenderRegistry$get_entry("IBCF", dataType="realRatingMatrix")

UserArtists <- read.csv("C:/Users/ckadic/Desktop/RecommendationTools/GroupAssignmentData/user_artists.dat", header=TRUE, sep="\t")
UserArtistsMatrix <- as(UserArtists,"matrix")
dim(UserArtistsMatrix)

### Prepare data ###
####################

train <- UserArtists[1:1050,]
test <- UserArtists[1051:2100,]


### Run model ###
#################

recom <- Recommender(train, method = "IBCF")
recom


### Recommend for one user ###
##############################

#u178
user = '178'
testUser <- UserArtists[user,]

predUser <- predict(recom, testUser, n = 3)

getList(predUser)
predUser@ratings

cat(UserArtists[getList(bestN(predUser, 3))[[1]]], sep = "\n\n")

#200
user = '200'
testUser <- UserArtists[user,]

predUser <- predict(recom, testUser, n = 3)

getList(predUser)
predUser@ratings

cat(UserArtists[getList(bestN(predUser, 3))[[1]]], sep = "\n\n")

### Recommend for all users ###
###############################

pred <- predict(recom, test, n = 3)

getList(pred)
pred@ratings

cat(UserArtists[getList(bestN(pred, 3))[[1]]], sep = "\n\n")


### Change parameters ###
#########################

recom2 <- Recommender(train, method = "IBCF", parameter = list(method = 'euclidean', nn=10, normalize = "z-score"))
recom2

pred <- predict(recom2, test, n = 3)

getList(pred)
pred@ratings

cat(UserArtists[getList(bestN(pred, 3))[[1]]], sep = "\n\n")

## All ratings
predRatings <- as(predict(recom2, test, n = 3, type = "ratings"), "matrix")