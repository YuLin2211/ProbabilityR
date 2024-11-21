


library(readxl)

data <- read_excel("matches.csv.xlsx")  #load future matches
teamStats <- read_excel("GoalCount.xlsx")  #load team performances

data <- as.data.frame(data)             #convert to data frame
teamStats <- as.data.frame(teamStats)

homeAd <- 1.18 ###########constant variable for home advantage
clubs <- unique(teamStats$Club)     #a whole column of club names


#########################MONTE CARLO BOUNDARY###############################
#A simulation starts : every match with Poisson outcome


finalScores <- data.frame(Club = clubs,Score = teamStats$CurrentScore)   ###final scores

matchesRecord <- data.frame(Home = data$Home, Away = data$Away, HomeGoal = NA,
                            AwayGoal = NA, HomeScore = NA, AwayScore = NA)
for(i in 1:nrow(data))
{
  currHome <- data[i,"Home"]
  currAway <- data[i,"Away"]
  homeIndex <- which(teamStats$Club == currHome)    #find the club that's playing
  awayIndex <- which(teamStats$Club == currAway)
  
  homeAverageGoal <- teamStats[homeIndex,"AverageGoal"]    #Average goals for both sides
  awayAverageGoal <- teamStats[awayIndex,"AverageGoal"]
  
  
  homeAttack <- teamStats[homeIndex,"Attack"]        #home club attack parameter
  awayDefense <- teamStats[awayIndex,"Defense"]      #away club defense parameter
  
  #Expected goal for home, lamda for Poisson
  homeExpectedGoal <- homeAverageGoal*homeAttack*awayDefense*homeAd
  
  awayAttack <- teamStats[awayIndex,"Attack"]        #away club attack parameter
  homeDefense <- teamStats[homeIndex,"Defense"]      #home club defense parameter
  
  
  #Expected goal for away, lamda for Poisson
  awayExpectedGoal <- awayAverageGoal*awayAttack*homeDefense
  
  homeGoals <- rpois(1,lambda = homeExpectedGoal)   #Poisson distributed home goals
  awayGoals <- rpois(1,lambda = awayExpectedGoal)   #Poisson distributed away goals
  
  scoreVector <- c(0,0)      #initialize outcome
  if(homeGoals>awayGoals){
    scoreVector <- c(3,0)          #home wins , earn 3 points
  }else if(homeGoals < awayGoals){
    scoreVector <- c(0,3)          #away wins, earn 3 points
  }else{
      scoreVector <- c(1,1)        #draw, both get 1 point
    }
  
  matchesRecord[i,"HomeGoal"] = homeGoals    # i works because mathches were directly copied from data
  matchesRecord[i,"AwayGoal"] = awayGoals
  matchesRecord[i,"HomeScore"] = scoreVector[1]
  matchesRecord[i,"AwayScore"] = scoreVector[2]
  
  finalScores[homeIndex,"Score"] <- finalScores[homeIndex,"Score"] + scoreVector[1]  #homeindex works because "finalScores" were directly copied from teamStats
  finalScores[awayIndex,"Score"] <- finalScores[awayIndex,"Score"] + scoreVector[2]  #same as above
}


# 1 simuation ends, continue to next round
########################MONTE CARLO BOUNDARY###########################
  
