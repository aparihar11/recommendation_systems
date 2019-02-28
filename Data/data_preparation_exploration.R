
#####loading required packages
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("recommenderlab")
#install.packages("lsa")
x<-c("data.table","dplyr","plotly","readr","recommenderlab", "ggplot2","tidyr","reshape2","jsonlite" ,"recommenderlab","lsa","wordcloud")
setwd("C:\\Users\\aparihar\\GitHub\\recommendation_systems\\Data")
lapply(x, require, character.only = TRUE)
source("ubcf_func.R")
source("ibcf_func.R")
source("cluster_func.R")

#####setting working directory


####content-based recommendation systems, 
####'user_taggedartists.dat' and 'tags.dat' data. 
#####To reshape the resulting data set, using the dcast function of the reshape2 package is an option.
user_tags <- read.delim("user_taggedartists.dat", header = TRUE)
tags <- read.delim("tags.dat", header = TRUE)
user_artists <- read.table("user_artists.dat", header = TRUE, sep = "", stringsAsFactors = FALSE)
artists <- read.delim("artists.dat", header = TRUE)
artists$name<-as.character(artists$name)

###join tag data for content filtering
artist_tags <- inner_join(user_tags, tags, by="tagID")
artist_tags_matrix<-as(artist_tags,"matrix")




user_artists_wide = user_artists %>% spread(key=artistID,value=weight)
dim(user_artists_wide)

#Create character Id
artists$charid=paste0("I",artists$id)

userids=user_artists_wide$userID
user_artists_wide$IuserID = NULL
rownames(user_artists_wide) = paste0("U",userids)
colnames(user_artists_wide) = paste0("I",colnames(user_artists_wide))
user_artists_wide[1:6,1:10]

####Filter, center and Scale data
#Select Top 1000 
visits_byitem=colSums(user_artists_wide[,-1],na.rm = T)

visits_1k = user_artists_wide[,order(visits_byitem,decreasing = T)[1:1000]]
num_visits=apply(visits_1k,1,function(x) return(sum(!is.na(x))))
visits_1k = visits_1k[num_visits>10,]
dim(visits_1k)
visits_1k=t(scale(t(visits_1k))[,])



###join artists data
user_artists_cluster<-inner_join(user_artists,artists, by= c("artistID"="id"))
user_artists_cluster<-as(user_artists_cluster,"Matrix")

#####DATA EXPLORATION

#####Most Popular Artists, No. Listeners
popular_artists <- user_artists%>%
  group_by(name) %>%
  count(name) %>%
  arrange(desc(n)) %>%
  top_n(5, n)
head(popular_artists)

  ####TOP TAGS
###Descriptive Tags for Artists
###Here are the top 200 most popular tags associated with artists in our data:

set.seed(1)
tagfreq<- count(artist_tags, tagValue)
head(tagfreq)
wordcloud(words = tagfreq$tagValue, freq = tagfreq$n, min.freq = 20,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#############################UBCF#############################################################

library(recommenderlab)
visits_1k_rrm=as(as.matrix(visits_1k),"realRatingMatrix")
set.seed(100)
eval_sets <- evaluationScheme(data = visits_1k_rrm, method = "split", train = .8, given = 10, goodRating=3, k = 1)

getArtistName=function(artistid){
  return(artists$name[artists$charid==artistid])
}
#Vectorize the func♦tion to enable get values for a vector of ids
getArtistName=Vectorize(getArtistName)

train=as.matrix(getRatingMatrix(getData(eval_sets,"train")))
test=as.matrix(getRatingMatrix(getData(eval_sets,"known")))
#realRating Matrix stores missing value as 0, convert them back to NA
train[train==0]=NA
test[test==0]=NA
#Check prediction for one user
pred1=ubcf_recommend(train,test[4,])
cat("\n**** Recommendation for user",rownames(test)[4],"\n")

recommend=getArtistName(colnames(train)[pred1$recommendation])
cat(recommend,sep="\n")

###########################################################################
###########################################################################

#####################Machine Learning- Collaborative Filtering#####################################
###########################################################################
###########################################################################


user_artists_wide = user_artists %>% spread(key=artistID,value=weight)
dim(user_artists_wide)


#Create character Id
artists$charid=paste0("I",artists$id)

userids=user_artists_wide$userID
user_artists_wide$IuserID = NULL
rownames(user_artists_wide) = paste0("U",userids)
colnames(user_artists_wide) = paste0("I",colnames(user_artists_wide))
user_artists_wide[1:6,1:10]

####Filter, center and Scale data
#Select Top 1000 
visits_byitem=colSums(user_artists_wide[,-1],na.rm = T)


visits_1k = user_artists_wide[,order(visits_byitem,decreasing = T)[1:1000]]
num_visits=apply(visits_1k,1,function(x) return(sum(!is.na(x))))
visits_1k = visits_1k[num_visits>10,]
dim(visits_1k)
visits_1k=t(scale(t(visits_1k))[,])


library(recommenderlab)
visits_1k_rrm=as(as.matrix(visits_1k),"realRatingMatrix")
set.seed(100)
eval_sets <- evaluationScheme(data = visits_1k_rrm, method = "split", train = .8, given = 10, goodRating=3, k = 1)

getArtistName=function(artistid){
  return(artists$name[artists$charid==artistid])
}
#Vectorize the function to enable get values for a vector of ids
getArtistName=Vectorize(getArtistName)

train=as.matrix(getRatingMatrix(getData(eval_sets,"train")))
test=as.matrix(getRatingMatrix(getData(eval_sets,"known")))
#realRating Matrix stores missing value as 0, convert them back to NA
train[train==0]=NA
test[test==0]=NA
#Check prediction for one user
pred1=ubcf_recommend(train,test[4,])
cat("\n**** Recommendation for user",rownames(test)[4],"\n")

recommend=getArtistName(colnames(train)[pred1$recommendation])
cat(recommend,sep="\n")


############Machine Learning-Cluster Based##################################
############################################################################
source("cluster_func.R")

na.omit(visits_1k)
visits_1k[!is.finite(visits_1k)] <- 0


cluster<-ClusterBasedCF(visits_1k,center=20,iter=10)
prediction<-cluster$prediction
cluster_topn<-cluster$topN






###################Machine Learning-IBCF################################
##########################################################################
###########################################################################


########################################################################################################################################

### Split in train and test ###

ua_spread <- user_artists %>%
  mutate(logplay = log(weight)) %>%
  select(userID, artistID, logplay) %>%
  spread(artistID, logplay)

matrix_wide <- as.matrix(ua_spread[, 2:ncol(ua_spread)])
rownames(matrix_wide) <- ua_spread$userID

# create realRatingMatrix, a data format used by recommenderlab
ratings_matrix <- as(matrix_wide, "realRatingMatrix")


#image(ratings_matrix[1:100, 1:100], main = "Visualization of Ratings Matrix")

ratings_matrix_redux <- ratings_matrix[,colCounts(ratings_matrix) >= 10]
ratings_matrix_redux <- ratings_matrix_redux[rowCounts(ratings_matrix_redux) >= 20]
ratings_matrix_redux<-as(ratings_matrix_redux,"matrix")

###############################

train <- ratings_matrix_redux[1:1200,]
test <- ratings_matrix_redux[1201:1549,]

ResultsIBCF <- ItemBasedCF(train, test, 3, NN=10, onlyNew=TRUE)

prediction <- as.data.frame(ResultsIBCF$prediction)

TopN <- as.data.frame(ResultsIBCF$topN)

#recmodel_ibcf




#Item 10

Item10 = ItemBasedCF(train, test, N=10, NN=10, onlyNew=TRUE)

#print("user = 2.893 / system = 0.757 / elapsed = 3.650")


####################################################################################################################
########################################Content Based Recommendation System###########################################
#####################################################################################################################
source(content_func.R)

#extract the 100 most frequently used tags
#the 100th most used tag has 315 tags
top_tags <- user_tags %>%
  group_by(tagID) %>%
  summarise(tag_count = n()) %>%
  arrange(desc(tag_count)) %>%
  slice(1:100)


#pair the artist-tag list down to the top 100 tags
#60.7% of data remains (113247/186479 tags)
artist_tags <- inner_join(user_tags, top_tags, by="tagID")

#item (artist)
artist_pro_data <- artist_tags %>% 
  select(artistID, tagID) %>%
  group_by(artistID, tagID) %>%
  summarise( count = n()) %>%
  arrange(desc(count)) %>%
  as.data.frame()

#spread out the data by having each tag (genre) be a separate column                     
artist_matrix <- spread(artist_pro_data, tagID, count)


#number of distinct users who rated each artist
distinct_artist_user_count <- artist_tags %>%
  group_by(artistID) %>%
  summarise(unique_users = n_distinct(userID)) %>%
  arrange(desc(unique_users))

#divide each row in artist matrix by the corresponding value in distinct_artist_user_count

artist_matrix <- inner_join(artist_matrix,distinct_artist_user_count)


CB <- ContentBased(artist_matrix, as(artist_matrix, "matrix"), 3, 10, onlyNew=T)


for (i in 1:nrow(artist_matrix)) {
  norm_artist_matrix[i,2:101] <- artist_matrix[i,2:101]/artist_matrix[i,102]
}

norm_artist_matrix0 <- norm_artist_matrix

norm_artist_matrix0[is.na(norm_artist_matrix0)] <- 0

madonna <- as.numeric(as.vector(norm_artist_matrix0[62,2:101]))
brittany_spears <- as.numeric(as.vector(norm_artist_matrix0[289,2:101]))
cher <- as.numeric(as.vector(norm_artist_matrix0[336,2:101]))
kylie_minogue <- as.numeric(as.vector(norm_artist_matrix0[51,2:101]))
depeche_mode <- as.numeric(as.vector(norm_artist_matrix0[67,2:101]))
lady_gaga <- as.numeric(as.vector(norm_artist_matrix0[84,2:101]))
radiohead <- as.numeric(as.vector(norm_artist_matrix0[124,2:101]))
talking_heads <- as.numeric(as.vector(norm_artist_matrix0[521,2:101]))
david_bowie <- as.numeric(as.vector(norm_artist_matrix0[599,2:101]))
wu_tang <- as.numeric(as.vector(norm_artist_matrix0[3197,2:101]))

cosine(madonna, kylie_minogue)
cosine(madonna,radiohead)

pred <- CB$prediction
pred
topN <- CB$topN
topN











