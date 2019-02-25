#####################################################################
####### Explanation of diferent data sets used in this course #######
#####################################################################

install.packages("arules")
install.packages("recommenderlab")
install.packages("lazy")
install.packages("Rmpfr") # select n

library(recommenderlab)

## Set wd to location where you 
wd = "/Users/ckadic/Desktop/RecommendationTools/GroupAssignmentData/"
setwd(wd)
getwd()

####### Informative Datasets ########
artists <- read.csv('Artists.dat', sep="\t")
tags <- read.csv('tags.dat', sep="\t")
UserArtists <- read.csv('user_artists.dat', sep="\t")
UserTaggedArtists <- read.csv('user_taggedartists.dat', sep="\t")

####### Datasets for recommendation creation ########
artists <- read.csv('Artists.dat', sep="\t")
tags <- read.csv('tags.dat', sep="\t")
UserArtists <- read.csv('user_artists.dat', sep="\t")
UserTaggedArtists <- read.csv('user_taggedartists.dat', sep="\t")

####### Statistics UserArtists ########

min(UserArtists$weight, na.rm=TRUE)
max(UserArtists$weight, na.rm=TRUE)
mean(UserArtists$weight, na.rm=TRUE)
median(UserArtists$weight, na.rm=TRUE)

hist(UserArtists$weight)

hist(UserArtists$weight[UserArtists$weight <= 1000])

## Unique users and artists
u = length(unique(UserArtists$userID))
u
i = length(unique(UserArtists$artistID))
i

# sparsity
1- (nrow(UserArtists)/(u*i))
# 0.9972172% missing values

####### Creation of the new dataset ########

UserTagsArtistsNew <- merge(UserTaggedArtists,tags,by="tagID")
head(UserTagsArtistsNew)

####### Reshaping UserTagsArtistsNew ########

install.packages("reshape2")
library(reshape2)
dcast(UserTagsArtistsNew, userID ~ year, fun.aggregate = length)

### Create Content-based matrix ###
###################################
m <- matrix(c(c(0,1,0,1,1), c(1,0,1,0,0), c(1,0,0,0,0), c(0,1,0,0,0), c(0,0,1,0,1),  c(0,0,0,1,0), c(1,1,1,1,00, c(0,0,0,0,1))), nrow=5)

rownames(m) <- c('MP', 'J', 'MS', 'S', 'WS')
colnames(m) <- c('Women', 'Men', 'Pants', 'Jacket', 'Suit', 'Shoe', 'Dark', 'Light')

### Calculate similarity between items ####
###########################################
library(proxy)
similarity_matrix <- as.matrix(simil(m, method="cosine"))

similarity_matrix[is.na(similarity_matrix)] <- 0

### Create behavior matrix ###
##############################
ratings <- matrix(c(c(5,4,1,3,5), c(1,3,4,4,2), c(5,3,2,1,4), c(2,1,5,5,1), c(0,1,4,5,1)), nrow=5)

colnames(ratings) <-c('MP', 'J', 'MS', 'S', 'WS')
rownames(ratings) <- c("O", "P", "M", "B", "C")

### Make predictions ###
########################
predictions  <- t(t(ratings %*% similarity_matrix) / colSums(similarity_matrix, na.rm=T))







