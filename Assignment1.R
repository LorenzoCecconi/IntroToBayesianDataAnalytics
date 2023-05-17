# Assignment 1 - Introduction to Bayesian Data Analysis
# Lorenzo Cecconi - 03778923

rm(list=ls())
library(tidyverse)

### Task1 ###

dice1 <- c(seq(1,6,1)) # sample space of dice 1
dice2 <- c(seq(1,6,1)) # sample space of dice 2

### Task 2 ###

obs <- data.frame(dice1, dice2) # creating data frame with the outcomes of the two dices
obs <- expand.grid(obs) # changing the data frame with all the 36 combinations throwing both dices

### Task 3 ###

probability <- 1/length(obs$dice1) # probability of each outcome, they're all the same as they have the same probability of occurance
obs <- cbind(obs, probability) # adding the probability of each outcome to the data frame

### Task 4 ###

sum <- obs[,1] + obs[,2] # creating vector of the sums
obs <- cbind(obs, sum) # adding the column of sums to the data frame
obs # prints the data frame

### Task 5 ###

P_dice1_is_3 <- sum(obs$probability[obs$dice1==3]) # calculates the probability of getting a 3 on the first dice
P_dice1_and_sum <- sum(obs$probability[obs$dice1==3 & obs$sum>=7]) # calculates the probability of getting a 3 on the first throw and that the sum of dice1 + dice2 is >=7
P_sum_given_dice1 = P_dice1_and_sum / P_dice1_is_3 # calculating the probability of sum >= 7 given dice1=3, using the conditional probability formula
P_sum_given_dice1 # prints the probability

# other solution for Task 5 creating the subset 
obs3 <- subset(obs, dice1 == 3) # creates subset of obs with dice1 == 3
prob = sum(obs3$probability[obs3$sum>=7])/sum(obs3$probability) # calculates the probability of having sum >=7 considering only the subset
prob # prints the probability

### Task 6 ###

P_betw_4_9 <- sum(obs$probability[obs$sum > 4 & obs$sum < 9]) # sums the probabilities of occurance of throws where the sum is between 4 and 9 (excluded)
P_betw_4_9 # prints the probability

# in case we want to include also 4 and 9 the code will be the following
P_betw_4_9_included <- sum(obs$probability[obs$sum >= 4 & obs$sum <= 9]) 
P_betw_4_9_included # prints the probability including 4 & 9
# The probability in this case will be .75 (excluding 4 and 9 is .5555556)

### Task 7 ###

data <- data.frame(matrix(nrow = 12, ncol = 2)) # creates a data frame 12X2 
names(data) <- c('Sum', 'Probability') # change names to the data frame columns
data[1:12,1] <- c(seq(1,12,1)) # first column is the possible outcome from the throw of the two dices, so 1 to 12

  for (i in 1:12) {                   # a cicle is used to complete the data frame with the probabilities related to each possible outcome
    obs_i <- subset(obs, sum == i)    # considers only the subset of outcomes where the sum is equal to i
    prob_i <- sum(obs_i$probability)  # sums the probabilities of obtaining a sum equal to i
    data[i,2] <- prob_i               # adds the probability of obtaining the sum i in the data frame
  }
max_prob <- max(data$Probability) # probability of the most probable sum
print(data$Sum[data$Probability == max_prob]) # prints the most probable sum
max_prob # prints the probability relative to the most probable sum

### Task 8 ###

Prob_delay <- data.frame(matrix(nrow = 11, ncol = 11)) # creates a data frame 11X11 where to put the probability distribution for each probability of delay 
names(Prob_delay) <- c('p=0', 'p=0.1', 'p=0.2','p=0.3', 'p=0.4', 'p=0.5','p=0.6', 'p=0.7', 'p=0.8', 'p=0.9', 'p=1') # renames the columns with the probabilities of delay taken in consideration
rownames(Prob_delay) <- c('0',"1","2","3","4","5","6","7",'8','9,','10') # renames the rows with the number of trains delayed
for (k in 0:10){      # cicle for to move along the columns, varying the probability of delay (0, .1, .2, ..., 1)
  for (i in 0:10){    # cicle for to move along the row, varying i : the number of trains delayed (0 to 10)
     Prob_delay[i+1,k+1] <-  dbinom(x = i, size = 10, prob = k*0.1) # fills the data frame by calculating the probability of occurance of a certain number i of trains in delay, given 10 trials (trains), and a probability of delay of each trial of k*0.1 
  }
}
Prob_delay # prints the data frame 

### Task 9 ###

sim_rides <- function(N, p){
  sample(c("L", "O"), size=N, replace=TRUE, prob=c(p, 1-p))
}
set.seed(1237)
obs_trains <- sim_rides(10, .3) # renamed the vector just to have a different name from the dice data frame
obs_trains

N <- length(obs_trains)
L <- sum(obs_trains == 'L') # counts the number of trains delayed in the given data
likelihood <- data.frame(matrix(nrow = 1, ncol = 11)) # creates a data frame where to store likelihood data
names(likelihood) <- c('p=0', 'p=0.1', 'p=0.2','p=0.3', 'p=0.4', 'p=0.5','p=0.6', 'p=0.7', 'p=0.8', 'p=0.9', 'p=1') # renames the data frame columns
rownames(likelihood) <- c('likelihood') # renames the data frame row
for (i in 0:10) {   # cicle for to fill the data frame with the likelihood relative to every different value of p
likelihood[,1+i] <- dbinom(x = L, size = N, p = 0.1*i) # calculates the likelihood of the data using the dbinom function, given size 10 and p from 0 to 1 (0, 0.1, 0.2, ..., 1)
}
likelihood # prints the likelihood values


### Task 10 ###

prior <- c(0.000, 0.004, 0.041, 0.123, 0.209, 0.246, 0.209, 0.123, 0.041, 0.004, 0.000) # prior probabilities
post <- prior*likelihood # calculates the posterior probability given the prior probability and the likelihood 
rownames(post) <- c('Post P') # renames the row in the post data frame
post # prints the posterior probability

post_norm <- post / sum(post) # normalazing the posterior probability so its sum is equal to 1
post_norm # prints the normalized posterior probability

