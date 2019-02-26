########################################################################################################################################
### Item-based CF as a function ###
###################################
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

ItemBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  # Similarity
  
  similarity_matrix <- as.matrix(simil(t(train_data), method="pearson"))
  
  print("Similarity calculation done")
  # Nearest Neighbor
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:ncol(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[,k])[NN]
    similarity_matrix_NN[,k] <- ifelse(similarity_matrix_NN[,k] >= crit_val, similarity_matrix_NN[,k], NA)
  }
  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
  
  train_data[is.na(train_data)] <- 0
  
  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0
  
  print("Nearest neighbor selection done")
  
  ### Prediction ###
  prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
  
  for (u in rownames(test_data)){
    # Numerator
    Num <-  test_data2[u, ] %*% similarity_matrix_NN
    
    # Denominator
    Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
    
    # Prediction
    prediction[u, ] <- Num/Denom
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}

########################################################################################################################################

### Split in train and test ###
###############################
UserArtistsMatrix <- as(UserArtists,"matrix")
train <- UserArtistsMatrix[1:1050,]
test <- UserArtistsMatrix[1051:2100,]

### Recommend for one user ###
##############################
#u2841 & u238

u1 = 'u2100'
u2 = 'u238'

tm <- matrix(, nrow=2, ncol=ncol(test), dimnames=(list(list(u1, u2), colnames(test))))
tm[1, ] <- t(as.matrix(train[u1,]))
tm[2, ] <- t(as.matrix(train[u2,]))

ResultsIBCFUser <- ItemBasedCF(train, tm, 3, NN=10, onlyNew=TRUE)

predictionUser <- as.data.frame(ResultsIBCFUser$prediction)

TopNUser <- as.data.frame(ResultsIBCFUser$topN)

for (u in 1:nrow(TopNUser)){
  for (i in 1:3){
    print(paste("Recommendation", i, "for user", u, ":"))
    cat(UserArtists[as.character(TopNUser[[i]])[u]], sep = "\n\n")
    print("________________")
  }
}

### Recommend for all test users ###
####################################

ResultsIBCF <- ItemBasedCF(train, test, 3, NN=10, onlyNew=TRUE)

prediction <- as.data.frame(ResultsIBCF$prediction)

TopN <- as.data.frame(ResultsIBCF$topN)
