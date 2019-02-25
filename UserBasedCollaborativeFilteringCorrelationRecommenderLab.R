#######################
### User - based CF with RecommenderLab ###
#######################   

# https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf

library(recommenderlab)

### Read In data ###
####################
wd = "/Users/ckadic/Desktop/RecommendationTools/GroupAssignmentData/"
setwd(wd)
getwd()
UserArtists <- read.csv('user_artists.dat', sep="\t")
UserArtistsMatrix <- as(UserArtists,"matrix")
dim(UserArtistsMatrix)
UserArtistsMatrix2 <- aggregate(weight ~ userID, data=UserArtistsMatrix, FUN=sum)

r <- recommenderRegistry$get_entry("UBCF", dataType="realRatingMatrix")

### Prepare data ###
####################
train <- UserArtists[1:1050,]
test <- UserArtists[1051:2100,]

user = '200'
testUser <- UserArtists[user,]

### Run model ###
#################

rtr <- as(train, "realRatingMatrix")
rts <- as(test, "realRatingMatrix")


recom1 <- Recommender(rtr, method = "UBCF", parameter=NULL)
recom1

recom2 <- Recommender(rts, method = "UBCF", parameter=NULL)
recom2

### Recommend for one user ###
##############################

#u2000
user = 'u2000'
testUser <- UserArtists[user,]

predUser <- predict(recom2, newdata=rts, n = 3)

getList(predUser)
predUser@ratings

cat(UserArtists[getList(bestN(predUser, 2))[[3]]], sep = "\n\n")


#u238
user = '238'
testUser <- UserArtists[user,]

predUser <- predict(recom2, newdata=rts, n = 3)

getList(predUser)
predUser@ratings

cat(UserArtists[getList(bestN(predUser, 45))[[1]]], sep = "\n\n")

### Recommend for all users ###
###############################

pred <- predict(recom2, newdata=rts, n = 10)

getList(pred)
pred@ratings

### Change parameters ###
#########################

recom1 <- Recommender(rtr, method = "UBCF", parameter = list(method = 'euclidean', nn = 10, normalize = "z-score"))
recom1

pred2 <- predict(recom2, newdata=rts, n = 3)

getList(pred2)
pred2@ratings

cat(UserArtists[getList(bestN(pred2, 3))[[1]]], sep = "\n\n")

## All ratings
predRatings <- as(predict(recom2, newdata=rts, n = 3, type = "ratings"), "matrix")