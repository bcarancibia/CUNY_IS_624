
nba <- read.csv("/users/bcarancibia/CUNY_IS_624/FinalProject/nbaplayerdata.csv", header=TRUE)

View(nba)

####First look at descriptive stats

summary(nba)

unique(nba$pos)

#There are 7 NA for age, remove them
unique(nba$age)

unique(nba$bref_team_id)

#One thing to notice is that the largest amount for teams is TOT which means that is total for all teams(player played for more than one team that year)

summary(nba$age)

