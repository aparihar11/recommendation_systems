#######################################
### Evaluation: Prediction Accuracy ###
####################################### 
install.packages("recommenderlab")
library(recommenderlab)
#install.packages('proxy')
library('proxy')

### Read In data ###
####################

wd = "/Users/ckadic/Desktop/RecommendationTools/GroupAssignmentData/"
setwd(wd)
getwd()
UserArtists <- read.csv('user_artists.dat', sep="\t")
UserArtistsMatrix <- as(UserArtists,"matrix")
dim(UserArtistsMatrix)
########################################################################################################################################

### score a model ###
#####################

# Load Models
setwd("/Users/ckadic/Desktop/RecommendationTools/GroupAssignmentData/")
source("functions.R")


# Split train - Test
set.seed(2)
train_rows = sample(1:nrow(UserArtistsMatrix), 0.7*nrow(UserArtistsMatrix))

train <- UserArtistsMatrix[train_rows,]
test <- UserArtistsMatrix[-train_rows,]

### score item-based
#Item 10
st <- proc.time()
Item10 = ItemBasedCF(train, test, N=10, NN=10, onlyNew=TRUE)
(proc.time() - st)
#print("user = 2.893 / system = 0.757 / elapsed = 3.650")

#Item 15
st <- proc.time()
Item15 = ItemBasedCF(train, test, N=10, NN=15, onlyNew=TRUE)
(proc.time() - st)
#print("user = 2.878 / system = 0.738 / elapsed = 3.617")

### score user-based
#User 10
st <- proc.time()
User10 = UserBasedCF(train, test, N=10, NN=10, onlyNew=TRUE)
(proc.time() - st)
print("user = 462 / system = 95 / elapsed = 558")

#User 15
st <- proc.time()
User15 = UserBasedCF(train, test, N=10, NN=15, onlyNew=TRUE)
(proc.time() - st)
#print("user = 462 / system = 95 / elapsed = 558")


### Prediction Accuracy ###
###########################

MAE <- function(prediction, real){
  
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    MAE = sqrt( sum( (prediction - real)^2 , na.rm = TRUE ) / (nrow(prediction) * ncol(prediction)) )
    return(MAE)
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}

# MAE Item 10
MAE(Item10$prediction, test)
# MAE Item 15
MAE(Item15$prediction, test)

# MAE User 10
MAE(User10$prediction, test)

# MAE User 15
MAE(User15$prediction, test)

########################################################################################################################################
### Prediction Accuracy: Existing functions ###
###############################################
#install.package("Metrics")
library(Metrics)

test2 <- test
for (i in 1:ncol(test2)){
  test2[, i] <- ifelse(is.na(test2[, i]), mean(test2[,i],na.rm=T), test2[, i] )
}

mae(test2, Item10$prediction)
mae(test2, Item15$prediction)

mae(test2, User10$prediction)
mae(test2, User15$prediction)
########################################################################################################################################
### Prediction Accuracy in Recommenderlab ###
#############################################


algorithms <- list(
  POPULAR = list(name = "POPULAR", param = NULL),
  IBCF = list(name = "IBCF", param = NULL),
  UBCF = list(name = "UBCF", param = NULL),
  SVD = list(name = "SVD", param = NULL)
)

# Prediction Accuracy
es <- evaluationScheme(UserArtists, method="split", train=0.7,  k=1, given=-1)
es

ev <- evaluate(es, algorithms, type="ratings")
ev

avg(ev)

plot(ev)


# Cross validation
es <- evaluationScheme(UserArtists, method="cross-validation",  k=10, given=-1)
es

ev <- evaluate(es, algorithms, type="ratings")
ev

avg(ev)