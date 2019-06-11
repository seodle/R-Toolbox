
#Computation of a permutation test.The goal is to test whether the observed difference
#between the treatment and the control samples is really significative.

observed.treatment.sample <- c(19,22,25,26) #the observed scores for the treatment group
observed.control.sample <- c(23,33,40) #the observed scores for the control group

observed.mean.difference <- mean(observed.treatment.sample) - mean(observed.control.sample)
observed.mean.difference #the mean difference between our two observed groups

#Is this difference significative or due to chance?

whole.sample <- c(observed.treatment.sample,observed.control.sample) #put together the two samples

random.mean.differences <- vector() #this vector will be containing the mean differences of each loop iteration


for (i in 1:10000) { #compute 10000 times random differences in the whole sample
  
  random.sample <- sample(whole.sample) #randomized the whole sample
  random.treatment.sample <- random.sample[1:4] #selected new random treatment sample
  random.control.sample <- random.sample[5:7] #selected new random control sample
  random.mean.differences <- c(random.mean.differences, 
                               mean(random.treatment.sample)-
                                 mean(random.control.sample)) #for each iteration of the loop, the new mean difference is added to the vector
  
}

#We then compute how many times our observed difference is equal or greater than a random difference.

#percentage of occurrences superior or equal to observed mean difference
p.value = sum((observed.mean.difference >= random.mean.differences) == TRUE) / length(random.mean.differences)
p.value

#In the sample studied, the chance of having the observed difference (9) or greater is below the thresold of 0.05.
#Threfore, we cannot conclude that the treatment have a significative effect on our treatment group.

