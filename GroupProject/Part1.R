
num_P <- 1000 #number of P values calculated
experiment_size <- 1000 #number of simulations for each calculation of P
deltaT <- 0.01  #small time increment; also variance
numSteps <- 1/deltaT #number of random increments
p_values <- numeric(num_P)    #create an array of P value

for(i in 1:num_P){       #outer loop : gather P values

successCount <- 0

for(j in 1 : experiment_size){     #inner loop : compute P
  cumulationSum <- 0 #sum of Temperature
  
  deltaY1 <- rnorm(numSteps,mean=0,sd=sqrt(deltaT))   # random increments(follow normal distribution)
  cumulationSum <- sum(deltaY1)  #sum of the increments

if(cumulationSum>0)                 #count the successful experiments
{
  successCount <- successCount + 1      
}
  
}

p_values[i] <- successCount/experiment_size   #probabilty of above zero, stored in p_values
}

hist(p_values, breaks = 50, probability = TRUE, col = "orange",main="Distribution of P",xlab ="P",
     ylab="density")


num_T <- 1000 #number of Tmax calculated
deltaT <- 0.01  #small time increment; also variance
numSteps <- 1/deltaT #number of random increments
maxT <- numeric(num_T)   # array of Tmax

for(i in 1:num_T){   #calculate all the Tmaxs
curr_Tmax <- 0
curr_T <- 0
  for(j in 1:numSteps){    #calculate one Tmax
    deltaY2 <- rnorm(1,mean=0,sd=sqrt(deltaT))
    curr_T <- curr_T + deltaY2
    curr_Tmax <- max(curr_T,curr_Tmax)
  }
maxT[i] = curr_Tmax     #store result into array
}
hist(maxT, breaks = 50, probability = TRUE, col = "orange",main="Distribution of Tmax",xlab ="Tmax",
     ylab="density")
