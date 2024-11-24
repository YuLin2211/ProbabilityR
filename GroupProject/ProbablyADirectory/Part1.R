
num_P <- 1000 #number of P values calculated
experiment_size <- 1000 #number of simulations for each calculation of P
deltaT <- 0.1  #small time increment; also variance
numSteps <- 1/deltaT #number of random increments
p_values <- numeric(experiment_size)    #create an array of P value


for(j in 1 : experiment_size){     #compute P
  cumulationSum <- 0 #sum of Temperature
  successCount <- 0  #above zero time
  print(j/experiment_size) # progress marker
  for(i in 1 : numSteps){
  deltaY1 <- rnorm(1,mean=0,sd=sqrt(deltaT))   # random increments(follow normal distribution)
  cumulationSum <- cumulationSum + deltaY1  #sum of the increments
    if(cumulationSum>0)                 #count successful time period
    {
      successCount <- successCount + 1      
    }
  }
  p_values[j] <- successCount/numSteps   #probabilty of above zero, stored in p_values
}

hist(p_values, breaks = 50, probability = TRUE, col = "orange",main="Distribution of P",xlab ="P",
     ylab="density")


num_T <- 1000 #number of Tmax calculated
deltaT <- 0.00001  #small time increment; also variance
numSteps <- 1/deltaT #number of random increments
maxT <- numeric(num_T)   # array of Tmax

for(i in 1:num_T){   #calculate all the Tmaxs
  if(((i/num_T)*100)%%10 ==0)
    print(i/num_T)
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
