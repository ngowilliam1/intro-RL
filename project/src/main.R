# STAT 497 - Project
# Winter 2019
# Authors: Matteo Esposito, William Ngo, Spyros Orfanos
#-------------------------------------------------------------------

#install.packages("R6")
#install.packages("rstudioapi")
#R6 is a package designed to bring OOP to base R
library(R6)
library(rstudioapi)
#Set work directory to be current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Rewards
invalidMoveReward = -0.75 #ie: hitting a wall or going beyond the border
winningGameReward = 5
unseenStateReward = -0.05
seenStateReward = -0.30

# Actions Definition
# 1 will indicate moving right
# 2 will indicate moving down
# 3 will indicate moving left
# 4 will indicate moving up
# If an action is taken and the subsequent state would be a wall or beyond the borders, the rat will not move.

#The Mazes
# 1 Indicates where mouse is free to move
# 0 Indicates a wall
# 10 Indicates the end destination

# Rat Starting Position
ratStartingPosition = c(1,1)

#Maze 1: 8x8
maze1 = matrix(c(1.,  0.,  1.,  1.,  1.,  1.,  1.,  1.,
                 1.,  0.,  1.,  1.,  1.,  0.,  1.,  1.,
                 1.,  1.,  1.,  1.,  0.,  1.,  0.,  1.,
                 1.,  1.,  1.,  0.,  1.,  1.,  1.,  1.,
                 1.,  1.,  0.,  1.,  1.,  1.,  1.,  1.,
                 1.,  1.,  1.,  0.,  1.,  0.,  0.,  0.,
                 0.,  1.,  1.,  0.,  1.,  1.,  1.,  1.,
                 0.,  1.,  1.,  1.,  0.,  1.,  1.,  10),
               nrow=8, ncol=8)

#Maze 2: 15x15
maze2 = rbind(
  c(1,0,1,1,1,0,0,0,1,1,0,0,1,1,1),
  c(1,1,1,1,1,0,1,1,1,0,1,1,0,0,1),
  c(1,1,0,1,1,0,1,1,0,1,1,1,1,1,1),
  c(1,0,1,0,1,1,1,0,0,1,1,0,0,1,1),
  c(0,1,1,1,0,1,1,0,1,1,1,1,0,1,0),
  c(1,0,0,1,1,1,1,1,0,0,0,1,1,0,1),
  c(1,1,0,0,0,1,0,1,1,1,1,1,0,1,1),
  c(0,1,1,1,1,1,0,1,1,0,1,1,1,1,1),
  c(0,1,1,1,1,1,0,1,1,1,0,0,1,1,1),
  c(1,1,0,1,1,1,1,1,1,1,1,0,1,1,0),
  c(0,1,1,1,1,0,0,0,0,0,0,0,1,0,0),
  c(1,1,1,0,1,1,1,1,1,1,1,1,1,1,0),
  c(1,1,0,1,1,1,1,1,0,1,1,0,0,1,1),
  c(1,0,0,1,1,1,0,1,1,1,1,0,1,1,1),
  c(1,0,0,0,1,1,1,1,0,1,1,1,1,1,10)
)

#Select the which maze to use (Set to either maze1 or maze2)
original_maze = maze1
source("3_DrawMaze.R")

#Set Number of Episodes Per Run
nepis = 1000
#Set Number of Runs
nb_runs = 100
#Set Initial Epsilon
epsilon = 0.4
#Set if epsilon should be decreasing within each episode.
decreasingEpsilon = 3
#Set Learning Step Size (Alpha)
stepsize = 0.1
#Set the "n" in NStepSARSA
nparam = 10
#Initial Policy has prob 0.25 of chossing each action in all states.
InitialPolicy = matrix(data = 0.25, nrow = nb_states, ncol = 4)
#Initial estimate of state-action value function Q is identically 0. 
Q_0 = matrix(data = 0, nrow = nb_states, ncol = 4)


#Execute all of the following to obtain desired result
source("4b_SARSA.R")
source("4c_QLearning.R")
source("4d_nStepSARSA.R")


#Plotting episode rewards 
plot(1:nepis, y = AvgEpisodeReturnSARSA, type = "l", main=paste("Average Return at State 1 over",nb_runs,"Runs With Exponentially Decaying Epsilon"), xlab = "Episode", ylab = expression(G[0]), col = "red")
lines(x=1:nepis, y= AvgEpisodeReturnQLearning, type="l",col="blue")
lines(x=1:nepis, y= AvgEpisodeReturnNStepSARSA, type="l",col="green")
lines(rep(3.85, nepis), col = "orange")
legend(x=500, y=-150 , cex = 0.8,
  c("SARSA","Q-Learning",paste(nparam,"-Step SARSA",sep=""),"Optimal Return at State 1"),
  fill=c("red","blue","green","orange"), bty = "n" )


#Average time to complete one run 
print(SARSATime / nb_runs) 
print(QLearningTime / nb_runs)
print(nStepTime / nb_runs)

# (Optional) The following displays the statistics of a 10000 episodes using a random policy, episodes end if they don't find the exit within 400 moves. (2-3 Minutes of Run-time)
source("4a_RandomPolicy.R")
randomPolicyStatistics

# (Optional) Compares the different epsilons over 50 runs of 1000 episodes, such as constant, constant/n, constant/log(n+1), exponentially decay
source("5a_EpsilonComparison")

# (Optional) Compares nStepSARSA at 3 different N-levels; 5, 10 and 15 over 50 runs of 1000 episodes
source("5b_nStepSARSAComparison")


