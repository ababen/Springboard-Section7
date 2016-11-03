setwd("~/R/Springboard-Section7/Logistic Regression")

library(readr)
songs <- read_csv("~/R/Springboard-Section7/Logistic Regression/songs.csv")
MichaelJackson = subset(songs, artistname == "Michael Jackson")
nrow(MichaelJackson)
MichaelJackson$Top10[13]
table(songs$timesignature)
which.max(songs$tempo) 
songs$songtitle[6206]
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
nrow(SongsTrain)
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)
