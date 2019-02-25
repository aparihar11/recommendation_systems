#######################
### User - based CF ###
#######################   
library(recommenderlab)
library(moments)
library(e1071)  
install.packages("psych")
library((psych))
install.packages("cluster")
library(cluster)
install.packages("fpc")
library(fpc)
install.packages("tidyr")
library((tidyr))

### Step 0. Read In data ###
############################

wd = "/Users/ckadic/Desktop/RecommendationTools/GroupAssignmentData/"
setwd(wd)
getwd()
UserArtists <- read.csv('user_artists.dat', sep="\t")
UserArtistsMatrix <- as(UserArtists,"matrix")
dim(UserArtistsMatrix)
UserArtistsMatrix2 <- aggregate(weight ~ userID, data=UserArtistsMatrix, FUN=sum)


########################################################################################################################################

### UBCF explained - Step 1: Compute cosine similarity ###
##########################################################

### The hard way ###
ptm <- proc.time()

# Initialize empty similarity matrix
similarity_matrix_THW <- matrix(, nrow = nrow(UserArtistsMatrix2), ncol = nrow(UserArtistsMatrix2), 
                                dimnames = list(rownames(UserArtistsMatrix2), rownames(UserArtistsMatrix2)))

# Calculate cosine similarity between all user pairs
for(i in 1:nrow(UserArtistsMatrix2)) {
  for(j in 1:nrow(UserArtistsMatrix2)) {
    if (i < j){
      # https://en.wikipedia.org/wiki/Matrix_multiplication
      sim <- sum(UserArtistsMatrix2[i, ]*UserArtistsMatrix2[j,], na.rm=TRUE)/sqrt(sum(UserArtistsMatrix2[i, ]^2, na.rm=TRUE) * sum(UserArtistsMatrix2[j, ]^2, na.rm=TRUE))
      similarity_matrix_THW[i,j] <- sim
      similarity_matrix_THW[j,i] <- sim
    }
  }
}


Time <- (proc.time() - ptm)
Time
# My Mac = +/- 260 sec

### Make use of R packages ###

#install.packages('proxy')
library('proxy')

ptm <- proc.time()

similarity_matrix <- as.matrix(simil(UserArtistsMatrix2, method="cosine"))

Time <- (proc.time() - ptm)
Time
# My Mac = +/- 12 sec

### UBCF explained - Step 2: Retain nearest neighbors ###
#########################################################

### Keep nearest neighbors based on similarity threshold ###
hist(similarity_matrix)

threshold = 0.5

similarity_matrix_threshold <- similarity_matrix
similarity_matrix_threshold[similarity_matrix_threshold < threshold] <- NA

### Keep N nearest neighbors ###
NN=10

similarity_matrix_NN <- similarity_matrix

for (k in 1:nrow(similarity_matrix_NN)){
  crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
  similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], NA)
}


### UBCF explained - Step 3: Prediction ###
###########################################
N = 3

prediction <- matrix(, nrow=nrow(UserArtistsMatrix2), ncol(UserArtistsMatrix2), dimnames=list(rownames(UserArtistsMatrix2), colnames(UserArtistsMatrix2)))
TopN <- matrix(, nrow=nrow(UserArtistsMatrix2), N, dimnames=list(rownames(UserArtistsMatrix2)))
#Preparation: Compute (r - mean(ri))

for (u in rownames(UserArtistsMatrix2)){
  similarity_vector <- na.omit(similarity_matrix_NN[u, ])
  
  NN_norm <- UserArtistsMatrix2[rownames(UserArtistsMatrix2) %in% names(similarity_vector),]
  
  CM <- colMeans(UserArtistsMatrix2, na.rm=TRUE)
  for (l in 1:ncol(NN_norm)){
    NN_norm[,l] <- NN_norm[,l] - CM[l]
  }
  NN_norm[is.na(NN_norm)] <- 0
  
  # Numerator
  Num = similarity_vector %*% NN_norm
  
  #Prediction
  prediction[u, ] =  mean(UserArtistsMatrix2[u, ], na.rm=TRUE)  + (Num/sum(similarity_vector, na.rm=TRUE))
  TopN[u, ] <- names(sort(-prediction[u, ]))[1:N]
}

########################################################################################################################################

### UBCF as a function - User-based CF for one user ###
#######################################################

UserBasedCFOneUser <- function(UserArtists, user, N, NN, onlyNew=TRUE){
  
  ### similarity ###
  similarity_vect <- vector(, nrow(UserArtists))
  names(similarity_vect) <- rownames(UserArtists)
  for (i in rownames(UserArtists)){
    if (i != user){
      sim <- sum(UserArtists[user, ]*UserArtists[i,], na.rm=TRUE)/sqrt(sum(UserArtists[user, ]^2, na.rm=TRUE) * sum(UserArtists[i, ]^2, na.rm=TRUE))
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

train <- UserArtistsMatrix2[1:1050,]
test <- UserArtistsMatrix[1051:2100,]

### Recommend for one user ###
##############################

#200
res <- UserBasedCFOneUser(train, user='200', N=10, NN=10, onlyNew=TRUE)
res
cat(UserArtists[names(res)], sep = "\n\n")

#238
res <- UserBasedCFOneUser(train, user='238', N=10, NN=10, onlyNew=TRUE)
res
cat(UserArtists[names(res)], sep = "\n\n")

### Recommend for all test users ###
####################################

ResultsUBCF <- UserBasedCF(train, test, N=3, NN=10, onlyNew=TRUE)

ResultsUBCF$prediction
ResultsUBCF$topN