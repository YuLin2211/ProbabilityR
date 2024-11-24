library(readxl)

data <- read_excel("matches.csv.xlsx")  #load future matches
teamStats <- read_excel("GoalCount.xlsx")  #load team performances

data <- as.data.frame(data)             #convert to data frame
teamStats <- as.data.frame(teamStats)

homeAd <- 1.18 ###########constant variable for home advantage
clubs <- unique(teamStats$Club)     #a whole column of club names
finalScores <- data.frame(Club = clubs,Score = teamStats$CurrentScore)   ###final scores

#########################MONTE CARLO BOUNDARY###############################
#A simulation starts : every match with Poisson outcome

num_simulations <- 100
all_results <- matrix(0, nrow = nrow(finalScores), ncol = num_simulations)
rownames(all_results) <- clubs

matchesRecord <- data.frame(
  Simulation = integer(),  # monte carlo tracking
  Home = character(),
  Away = character(),
  HomeGoal = integer(),
  AwayGoal = integer(),
  HomeScore = integer(),
  AwayScore = integer(),
  stringsAsFactors = FALSE
)

for(sim in 1:num_simulations){
  sim_finalScores <- data.frame(Club = clubs,Score = teamStats$CurrentScore)   ###final scores
  
  
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
    
    # Record match outcome in matchesRecord
    matchesRecord <- rbind(
      matchesRecord,
      data.frame(
        Simulation = sim,
        Home = currHome,
        Away = currAway,
        HomeGoal = homeGoals,
        AwayGoal = awayGoals,
        HomeScore = scoreVector[1],
        AwayScore = scoreVector[2]
      )
    )
    # update scores
    sim_finalScores[homeIndex, "Score"] <- sim_finalScores[homeIndex, "Score"] + scoreVector[1]
    sim_finalScores[awayIndex, "Score"] <- sim_finalScores[awayIndex, "Score"] + scoreVector[2]
  }
  
  # save result of this sim
  all_results[, sim] <- sim_finalScores$Score
}

# Average of all simulation
average_points <- rowMeans(all_results)

# Create final predicted table
final_table <- data.frame(Club = clubs, AvgPoints = average_points)
final_table$AvgPoints <- round(final_table$AvgPoints)
final_table <- final_table[order(-final_table$AvgPoints), ]

# 1 simuation ends, continue to next round
########################MONTE CARLO BOUNDARY###########################


# Save matchesRecord to CSV
write.csv(matchesRecord, "matchesRecord.csv", row.names = FALSE)

library(ggplot2)
ggplot(final_table, aes(x = reorder(Club, -AvgPoints), y = AvgPoints)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Predicted Average Points for Each Team") +
  xlab("Club") +
  ylab("Average Points")



#finalScoreTable
finalScoresTable <- finalScores[order(-finalScores$Score), ]  # Sort by Score descending
ggplot(finalScoresTable, aes(x = reorder(Club, -Score), y = Score)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  ggtitle("Final Predicted Scores for Each Team") +
  xlab("Team") +
  ylab("Final Predicted Score") +
  theme_minimal()


# Prepare data for boxplot
boxplot_data <- data.frame(
  Team = rep(rownames(all_results), each = num_simulations),
  Points = as.vector(all_results)
)
ggplot(boxplot_data, aes(x = reorder(Team, -Points), y = Points)) +
  geom_boxplot(fill = "red", color = "black") +
  coord_flip() +
  ggtitle("Distribution of Simulated Points for Each Team") +
  xlab("Team") +
  ylab("Points") +
  theme_minimal()

# Prepare attack vs defense data
attack_defense_data <- data.frame(
  Team = teamStats$Club,
  Attack = teamStats$Attack,
  Defense = teamStats$Defense
)

# Plot
ggplot(attack_defense_data, aes(x = Attack, y = Defense, label = Team)) +
  geom_point(size = 4, color = "black") +
  geom_text(vjust = -1, size = 3) +
  ggtitle("Attack vs Defense Comparison") +
  xlab("Attacking Strength") +
  ylab("Defensive Strength") +
  theme_minimal()



