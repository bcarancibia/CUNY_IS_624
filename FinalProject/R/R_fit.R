#setup the rscript
library("matrixcalc")
library("caTools")
library("randomForest")
library("caret")

#####YOU WILL NEED TO CHANGE THIS
setwd("/users/bcarancibia/CUNY_IS_624/FinalProject")
#########

#load data
nba1 = read.csv("data/scores_team_00213.csv") # team box scores 2013-14

summary(nba1)

summary(nba2)
nba2 = read.csv("data/scores_team_00214.csv") # team box scores 2014-15

nba <- rbind(nba1,nba2)

summary(nba)

#set opponent stats in each game
#what this does is show the stats of the oponent in the same row of data allows for easier field calculations
ngames = nrow(nba)
for (game in seq(1,ngames/2)){
  nba$opts[2*game-1] = nba$pts[2*game]
  nba$opts[2*game] = nba$pts[2*game-1]
  
  nba$ofgm[2*game-1] = nba$fgm[2*game]
  nba$ofgm[2*game] = nba$fgm[2*game-1]
  
  nba$ofga[2*game-1] = nba$fga[2*game]
  nba$ofga[2*game] = nba$fga[2*game-1]
  
  nba$o3pm[2*game-1] = nba$X3pm[2*game]
  nba$o3pm[2*game] = nba$X3pm[2*game-1]
  
  nba$o3pa[2*game-1] = nba$X3pa[2*game]
  nba$o3pa[2*game] = nba$X3pa[2*game-1]
  
  nba$oftm[2*game-1] = nba$ftm[2*game]
  nba$oftm[2*game] = nba$ftm[2*game-1]
  
  nba$ofta[2*game-1] = nba$fta[2*game]
  nba$ofta[2*game] = nba$fta[2*game-1]
  
  nba$oto[2*game-1] = nba$to[2*game]
  nba$oto[2*game] = nba$to[2*game-1]
  
  nba$otot[2*game-1] = nba$tot[2*game]
  nba$otot[2*game] = nba$tot[2*game-1]
}

#shooting percentages
nba$fgpct=nba$fgm/nba$fga * 100 #home team
nba$X3ppct=nba$X3pm/nba$X3pa * 100 #home team
nba$ftpct=nba$ftm/nba$fta * 100 #home team
nba$ofgpct=nba$ofgm/nba$ofga * 100 #away team
nba$o3ppct=nba$o3pm/nba$o3pa * 100 #away team
nba$oftpct=nba$oftm/nba$ofta * 100 #away team

#add wins
nba$win = nba$pts > nba$opts

#add home status
nba$ishome = (nba$tm == nba$home)

summary(nba)
str(nba)

#model1 <- glm(win ~ ., data=nba, family="binomial")
#rfImp1 <- varImp(model1, scale = FALSE)

#Warning the above will take a long time. Variables that are of interested are ishome, fgpct, ofpct, to, oto, tot, otot, win

#variables of interest
vars = c("ishome", "fgpct", "ofgpct", "to", "oto", "tot", "otot", "win") # 87.5%
nbaSub = nba[,vars]

# split into train/test data using 70% split ratio
split = sample.split(nbaSub$win, SplitRatio=0.7)
nbaTrain = subset(nbaSub, split==TRUE)
nbaTest = subset(nbaSub, split==FALSE)


# now predict wins for test data
nbaLog = glm(win ~ ., data=nbaTrain, family="binomial")
nbaPredict = predict(nbaLog, newdata=nbaTest, type="response")
cm = table(nbaTest$win, nbaPredict>0.5)
cm
plot(cm, col = "blue", main="Prediction Using GLM")
matrix.trace(cm) / sum(cm)
summary(nbaLog)

#reset values
nbaTest = nbaSub
nbaSub = nba[,vars]

split = sample.split(nbaSub$win, SplitRatio=0.7)
nbaTrain = subset(nbaSub, split==TRUE)
nbaTest = subset(nbaSub, split==FALSE)

# decision tree
library(rpart)
nbaMod = rpart(win ~ ., data=nbaTrain, method="class", minbucket=50)
library(rpart.plot)
prp(nbaMod) # plot tree
nbaPredict = predict(nbaMod, newdata=nbaTest, type="class")

plot(nbaMod, uniform=TRUE, 
     main="Classification Tree for NBA")
text(nbaMod, use.n=TRUE, all=TRUE, cex=.8)

summary(nbaPredict)

summary(nbaMod)


cm = table(nbaTest$win, nbaPredict)

print(cm)
plot(cm, col = "blue", main="Prediction Using Decision Tree")
matrix.trace(cm) / sum(cm)
summary(nbaMod)




