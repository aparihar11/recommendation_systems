#######################################
### Evaluation: Prediction Accuracy ###
####################################### 
library(recommenderlab)

### Read In data ###
source(ubcf_func.R)
source(ibcf_func.R)
source(hybrid.R)
source(cluster_func.R)
source(content_func.R)

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

RSME <- function(prediction, real){
  
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    RSME = sqrt( sum( (prediction - real)^2 , na.rm = TRUE ) / (nrow(prediction) * ncol(prediction)) )
    return(RSME)
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}

# RSME Item 10
RSME(Item10$prediction, test)
# RSME Item 15
RSME(Item15$prediction, test)

# RSME User 10
RSME(User10$prediction, test)

# RSME User 15
RSME(User15$prediction, test)

########################################################################################################################################
### Prediction Accuracy: Existing functions ###
###############################################
#install.package("Metrics")
library(Metrics)

test2 <- test
for (i in 1:ncol(test2)){
  test2[, i] <- ifelse(is.na(test2[, i]), mean(test2[,i],na.rm=T), test2[, i] )
}

RMSE(test2, Item10$prediction)
RMSE(test2, Item15$prediction)

RMSE(test2, User10$prediction)
RMSE(test2, User15$prediction)
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
es <- evaluationScheme(Jester5k, method="split", train=0.7,  k=1, given=-1)
es

ev <- evaluate(es, algorithms, type="ratings")
ev

avg(ev)

plot(ev)


# Cross validation
es <- evaluationScheme(Jester5k, method="cross-validation",  k=10, given=-1)
es

ev <- evaluate(es, algorithms, type="ratings")
ev

avg(ev)