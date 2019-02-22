#######################
### User - based CF with RecommenderLab ###
#######################   

# https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf

#install.packages("recommenderlab")
library(recommenderlab)
#install.packages("stats")
library("stats")

UserArtists <- read.csv("C:/Users/ckadic/Desktop/RecommendationTools/GroupAssignmentData/user_artists.dat", header=TRUE, sep="\t")
UserArtistsMatrix <- as(UserArtists,"matrix")
dim(UserArtistsMatrix)

### Read In data ###
####################
recommenderRegistry$get_entry("UBCF", dataType="realRatingMatrix")

### Prepare data ###
####################
train <- UserArtists[1:1050,]
test <- UserArtists[1051:2100,]

userID = '178'
testUser <- UserArtists[userID,]

### Run model ###
#################

recom <- Recommender(train, method = "UBCF")
recom

### Recommend for one user ###
##############################

#u200
userID = '200'
testUser <- UserArtists[userID,]

predUser <- predict(recom, testUser, n = 3)

getList(predUser)
predUser@ratings

cat(UserArtists[getList(bestN(predUser, 3))[[1]]], sep = "\n\n")


#u238
userID = '238'
testUser <- UserArtists[user,]

predUser <- predict(recom, testUser, n = 3)

getList(predUser)
predUser@ratings

cat(UserArtists[getList(bestN(predUser, 3))[[1]]], sep = "\n\n")

### Recommend for all users ###
###############################

pred <- predict(recom, test, n = 10)

getList(pred)
pred@ratings

### Change parameters ###
#########################

recom2 <- Recommender(train, method = "UBCF", parameter = list(method = 'euclidean', nn = 10, normalize = "z-score"))
recom2

pred2 <- predict(recom2, testUser, n = 3)

getList(pred2)
pred2@ratings

cat(UserArtists[getList(bestN(pred2, 3))[[1]]], sep = "\n\n")

## All ratings
predRatings <- as(predict(recom2, testUser, n = 3, type = "ratings"), "matrix")