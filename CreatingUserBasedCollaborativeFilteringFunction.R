### UBCF as a function - User-based CF for one user ###
#######################################################

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

UserBasedCFOneUser <- function(UserArtists, user, N, NN, onlyNew=TRUE){
  
  ### similarity ###
  similarity_vect <- vector(, nrow(UserArtists))
  names(similarity_vect) <- rownames(UserArtists)
  for (i in rownames(UserArtists)){
    if (i != user){
      sim <- sum(UserArtists[user, ]*dataset[i,], na.rm=TRUE)/sqrt(sum(UserArtists[user, ]^2, na.rm=TRUE) * sum(UserArtists[i, ]^2, na.rm=TRUE))
      similarity_vect[i] <- sim
    }
  }
  
  ### Nearest Neighbors ###
  crit_val <- -sort(-similarity_vect)[NN]
  similarity_vect <- na.omit(ifelse(similarity_vect >= crit_val, similarity_vect, NA))
  
  ### Prediction ###
  # Prepare
  NN_norm <- UserArtists[rownames(UserArtists) %in% names(similarity_vect),]
  CM <- colMeans(UserArtists, na.rm=TRUE)
  for (l in 1:ncol(NN_norm)){
    NN_norm[,l] <- NN_norm[,l] - CM[l]
  }
  NN_norm[is.na(NN_norm)] <- 0
  
  # Numerator
  Num = similarity_vect %*% NN_norm
  
  #Prediction
  prediction = mean(UserArtists[user, ], na.rm=TRUE) + (Num/sum(similarity_vect, na.rm=TRUE))
  names(prediction) = colnames(UserArtists)
  
  if (onlyNew == TRUE){
    unseen <- names(UserArtists[user, is.na(UserArtists[user,])])
    prediction <- prediction[names(prediction) %in% unseen]
  }
  TopN <- head(-sort(-prediction), N)
  
  return(TopN)
}

### UBCF as a function - User-based CF for all users ###
########################################################

UserBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  
  ### similarity ###
  similarity_matrix <- matrix(, nrow = nrow(test_data), ncol = nrow(train_data), 
                              dimnames = list(rownames(test_data), rownames(train_data)))
  
  for (i in rownames(test_data)){
    for (j in rownames(train_data)){
      sim <- sum(test_data[i, ]*train_data[j,], na.rm=TRUE)/sqrt(sum(test_data[i, ]^2, na.rm=TRUE) * sum(train_data[j, ]^2, na.rm=TRUE))
      similarity_matrix[i,j] <- sim
    }
  } 
  print("similarity calculation done")
  ### Nearest Neighbors ###
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:nrow(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
    similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], NA)
  }
  
  print("Nearest Neighbor selection done")
  ### Prediction ###
  # Prepare
  prediction <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  
  TopN <- matrix(, nrow=nrow(test_data), ncol=N, dimnames=list(rownames(test_data)))
  ### Numerator ###
  for (u in rownames(test_data)){
    similarity_vector <- na.omit(similarity_matrix_NN[u, ])
    
    NN_norm <- train_data[rownames(train_data) %in% names(similarity_vector),]
    
    CM <- colMeans(train_data, na.rm=TRUE)
    for (l in 1:ncol(NN_norm)){
      NN_norm[,l] <- NN_norm[,l] - CM[l]
    }
    NN_norm[is.na(NN_norm)] <- 0
    
    # Numerator
    Num = similarity_vector %*% NN_norm
    
    #Prediction
    prediction[u, ] =  mean(test_data[u, ], na.rm=TRUE)  + (Num/sum(similarity_vector, na.rm=TRUE))
    
    
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

### Using the functions ###
###########################

### Split in train and test ###
###############################

train <- UserArtistsMatrix[1:1050,]
test <- UserArtistsMatrix[1051:2100,]

### Recommend for one user ###
##############################

#200
res <- (train, user='u200', N=3, NN=10, onlyNew=TRUE)
res
cat(UserArtists[names(res)], sep = "\n\n")

#u238
res <- UserBasedCFOneUser(train, user='u238', N=3, NN=10, onlyNew=TRUE)
res
cat(UserArtists[names(res)], sep = "\n\n")

### Recommend for all test users ###
####################################

ResultsUBCF <- UserBasedCF(train, test, N=3, NN=10, onlyNew=TRUE)

ResultsUBCF$prediction
ResultsUBCF$topN
