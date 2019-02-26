###############################
### Evaluation ###
###############################    
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
#Item 15
st <- proc.time()
Item15 = ItemBasedCF(train, test, N=10, NN=15, onlyNew=TRUE)
(proc.time() - st)

### score user-based
# User 10
st <- proc.time()
User10 = UserBasedCF(train, test, N=10, NN=10, onlyNew=TRUE)
(proc.time() - st)
#print("user = 462 / system = 95 / elapsed = 558")

#User 15
st <- proc.time()
User15 = UserBasedCF(train, test, N=10, NN=15, onlyNew=TRUE)
(proc.time() - st)
#print("user = 462 / system = 95 / elapsed = 558")``


### Classification Accuracy ##
##############################

Classification <- function(prediction, real, threshold=NA, TopN=NA){
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    # Threshold #
    if (!is.na(threshold)){
      TP = sum(ifelse(prediction >= threshold & real >= threshold, 1, 0), na.rm=T)
      FP = sum(ifelse(prediction >= threshold & real < threshold, 1, 0), na.rm=T)
      FN = sum(ifelse(prediction < threshold & real >= threshold, 1, 0), na.rm=T)
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      Class_Thres = list(Recall, Precision)
      names(Class_Thres) = c("Recall", "Precision")
    }
    if (!is.na(TopN)){
      TP = vector(, length = nrow(prediction))
      FP = vector(, length = nrow(prediction))
      FN = vector(, length = nrow(prediction))
      
      for (u in nrow(prediction)){
        threshold_pred = -sort(-prediction[u, ])[TopN]
        threshold_real = -sort(-real[u, ])[TopN]
        TP[u] = sum(ifelse(prediction[u, ] >= threshold_pred & real[u, ] >= threshold_real, 1, 0), na.rm=T)
        FP[u] = sum(ifelse(prediction[u, ] >= threshold_pred & real[u, ] < threshold_real, 1, 0), na.rm=T)
        FN[u] = sum(ifelse(prediction[u, ] < threshold_pred & real[u, ] >= threshold_real, 1, 0), na.rm=T)
      }
      TP = sum(TP[u])
      FP = sum(FP[u])
      FN = sum(FN[])
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      Class_TopN = list(Recall, Precision)
      names(Class_TopN) = c("Recall", "Precision")
    }
    
    
    if (!is.na(threshold) & !is.na(TopN)){
      Class = list(Class_Thres, Class_TopN)
      names(Class) = c("Threshold", "TopN")
    }else if (!is.na(threshold) & is.na(TopN)) {
      Class = Class_Thres
    }else if (is.na(threshold) & !is.na(TopN)) {
      Class = Class_TopN
    }else{
      Class = "You have to specify the 'Threshold' or 'TopN' parameter!"
    }
    return(Class)  
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}

# Classification Item 10
Classification(Item10$prediction, test, threshold=5, TopN=5)
# Classification Item 15
Classification(Item15$prediction, test, threshold=5, TopN=5)

# Classification User 10
Classification(User10$prediction, test, threshold=5, TopN=5)
# Classification User 15
Classification(User15$prediction, test, threshold=5, TopN=5)


########################################################################################################################################
### Classification Accuracy in Recommenderlab ###
##################################################


algorithms <- list(
  POPULAR = list(name = "POPULAR", param = NULL),
  IBCF = list(name = "IBCF", param = NULL),
  UBCF = list(name = "UBCF", param = NULL),
  SVD = list(name = "SVD", param = NULL)
)

# Prediction Accuracy
es <- evaluationScheme(UserArtists, method="split", train=0.7,  k=1, given= 3, goodRating = 5)
es

ev <- evaluate(es, algorithms, type="topNList")

avg(ev)

plot(ev)

plot(ev, "prec/rec", annotate=T)