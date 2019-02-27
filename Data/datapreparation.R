
#####loading required packages
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("Recommenderlab")
#install.packages("lsa")
x<-c("data.table","dplyr","plotly","readr","recommenderlab", "ggplot2","tidyr","reshape2","jsonlite" ,"Recommenderlab","lsa","wordcloud")
lapply(x, require, character.only = TRUE)

#####setting working directory
setwd("C:\\Users\\aparihar\\Documents\\lastfm\\")

####content-based recommendation systems, 
####'user_taggedartists.dat' and 'tags.dat' data. 
#####To reshape the resulting data set, using the dcast function of the reshape2 package is an option.
user_tags <- read.delim("user_taggedartists.dat", header = TRUE)
tags <- read.delim("tags.dat", header = TRUE)
user_artists <- read.table("user_artists.dat", header = TRUE, sep = "", stringsAsFactors = FALSE)
artists <- read.delim("artists.dat", header = TRUE)

###join tag data for content filtering
artist_tags <- inner_join(user_tags, tags, by="tagID")



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
collab_data<-visits_1k
save(collab_data,file="collaboration_filter.Rdata")

###join artists data
name_artists<-inner_join(user_artists,artists, by= c("artistID"="id"))

#####DATA EXPLORATION

#####Most Popular Artists, No. Listeners
popular_artists <- name_artists%>%
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
  

