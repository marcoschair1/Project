library(stats)
library(dplyr)
library(randomForest)
library(RCurl)
library(caret)

#retrieve dataset from github/load
# x <- url("https://raw.github.tamu.edu/finvedy/SpotifyDataAnalysis/master/popularity_songs.csv?token=AAADTNRMBLFTZOWBRU7E4Q3AT2KYM")
genre_data <- read.csv("popularity_songs2.csv")

##removed outlier songs that are too long
duration_outliers <- boxplot(genre_data$duration_ms,plot = FALSE, range = 4)$out
playlist_songs_no_outliers <- genre_data %>%
  filter(!duration_ms %in% duration_outliers) %>%
  filter(!track.popularity < 3 )


#create dataframe with only label and predictors
plist_features <- playlist_songs_no_outliers[,c(10,4,12:15,17:23)] #loudness was not included due to correlation
plist_features$key <- as.factor(plist_features$key) #make key and mode predictors categorical
#plist_features$mode <- as.factor(plist_features$mode)
plist_features$playlist_genre <- as.factor(plist_features$playlist_genre)

#split data into train/test
dat.d = sample(1:nrow(plist_features),size = nrow(plist_features)*.8,replace = FALSE) 
train = plist_features[dat.d,]
test = plist_features[-dat.d,]

#random forest
rfm = randomForest(playlist_genre~.,data = train)
genre_predict = predict(rfm,test) #predict
test$genre_predict = genre_predict

#make confusion matrix and accuracy
cfm = confusionMatrix(test$playlist_genre,test$genre_predict)
cfm
rfm

plot(rfm)
#plot of variable importance
varImpPlot(rfm,
           sort = T,
           n.var = 12,
           main = 'Variable Importance')
varImp(rfm)

