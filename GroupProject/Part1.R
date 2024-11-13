
num_P <- 1000 #number of P values calculated
experiment_size <- 1000 #number of simulations for each calculation of P
deltaT <- 0.01  #small time increment; also variance
numSteps <- 1/deltaT #number of random increments
p_values <- numeric(num_P)    #create an array of P value

for(i in 1:num_P){       #outer loop : gather P values

successCount <- 0

for(j in 1 : experiment_size){     #inner loop : compute P
  cumulationSum <- 0 #sum of Temperature
  
  deltaY <- rnorm(numSteps,mean=0,sd=sqrt(deltaT))   # random increments(follow normal distribution)
  cumulationSum <- sum(deltaY)  #sum of the increments

if(cumulationSum>0)                 #count the successful experiments
{
  successCount <- successCount + 1      
}
  
}

p_values[i] <- successCount/experiment_size   #probabilty of above zero, stored in p_values
}

hist(p_values, breaks = 50, probability = TRUE, col = "orange",main="Distribution of P",xlab ="P",
     ylab="density")