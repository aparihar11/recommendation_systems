#Function that computes similarity
similarity_user=function(all_data,user_data){
  #Transpose so that users are on columns
  all_data=t(all_data)
  #Use pearson correlation
  score = cor(all_data,user_data,use = "pairwise")
  return(score)
}

#Function that predicts rating and list recommendation
ubcf_recommend=function(data,user_data,num_rec=20,num_sim=50){
  user_sim = similarity_user(data,user_data)
  #Replace NA with zero
  data_na0=data
  data_na0[is.na(data_na0)]=0
  top_sim_users=order(user_sim,decreasing = T)[1:num_sim]
  ratings=(user_sim[top_sim_users,1]%*%data_na0[top_sim_users,])
  prediction=NULL
  prediction$ratings=ratings
  #Set rating of already rated item to NA
  ratings[!is.na(user_data)]=NA
  prediction$recommendation=order(ratings,decreasing = T)[1:20]
  #To do remove already visited artists
  return(prediction)
}


getArtistName=function(artistid){
  return(artists$name[artists$charid==artistid])
}
#Vectorize the function to enable get values for a vector of ids
getArtistName=Vectorize(getArtistName)


# 
# 
# ########################################################################################################################################
# ### Item-based CF as a function ###
# ###################################
# 
# library(proxy)
# 
# ItemBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
#   # Similarity
#   
#   similarity_matrix <- as.matrix(simil(t(train_data), method="cosine"))
#   
#   print("Similarity calculation done")
#   # Nearest Neighbor
#   similarity_matrix_NN <- similarity_matrix
#   
#   for (k in 1:ncol(similarity_matrix_NN)){
#     crit_val <- -sort(-similarity_matrix_NN[,k])[NN]
#     similarity_matrix_NN[,k] <- ifelse(similarity_matrix_NN[,k] >= crit_val, similarity_matrix_NN[,k], NA)
#   }
#   similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
#   
#   train_data[is.na(train_data)] <- 0
#   
#   test_data2 <- test_data
#   test_data2[is.na(test_data2)] <- 0
#   
#   print("Nearest neighbor selection done")
#   
#   ### Prediction ###
#   prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
#                        dimnames=list(rownames(test_data), colnames(test_data)))
#   prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
#                         dimnames=list(rownames(test_data), colnames(test_data)))
#   TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
#   
#   for (u in rownames(test_data)){
#     # Numerator
#     Num <-  test_data2[u, ] %*% similarity_matrix_NN
#     
#     # Denominator
#     Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
#     
#     # Prediction
#     prediction[u, ] <- Num/Denom
#     
#     if (onlyNew == TRUE){
#       unseen <- names(test_data[u, is.na(test_data[u,])])
#       prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
#     }else{
#       prediction2[u, ] <- prediction[u, ]
#     }
#     
#     TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
#     
#   }
#   
#   print("Prediction done")
#   
#   res <- list(prediction, TopN)
#   names(res) <- c('prediction', 'topN')
#   
#   return(res)
# }
# 
# 
# 
# 
# 
# 
# 
# 
