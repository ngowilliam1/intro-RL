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
nb_runs = 50
#Set Learning Step Size (Alpha)
stepsize = 0.1
#Initial Policy has prob 0.25 of chossing each action in all states.
InitialPolicy = matrix(data = 0.25, nrow = nb_states, ncol = 4)
#Initial estimate of state-action value function Q is identically 0. 
Q_0 = matrix(data = 0, nrow = nb_states, ncol = 4)

# install.packages("grDevices")
# library(grDevices)

QLearning = function(stepsize, initQ, initPolicy, nepis, epsilon, theseed, decreasingEpsilon){
  #Q-Learning Algorithm 
  Qestim = initQ
  Policy = initPolicy
  EpisodeReturn = NULL
  for(n in 1:nepis){
    E = EpisodePolicy(n+theseed, Policy)
    EpisodeReturn[n] = sum(E[3,], na.rm = TRUE)
    
    for (tt in 1:(ncol(E)-1)){
      Qestim[E[1,tt], E[2,tt]] = Qestim[E[1,tt], E[2,tt]] + stepsize*(E[3,tt] + max(Qestim[E[1,tt+1],]) - Qestim[E[1,tt],E[2,tt]] )
    }
    #improving e-greedy policy
    if(decreasingEpsilon == 1){
      Policy = EGPolicy(epsilon/n, Qestim)  
    } else if (decreasingEpsilon == 2){
      Policy = EGPolicy(epsilon/log(n+1), Qestim) 
    } else if (decreasingEpsilon == 3){
      Policy = EGPolicy(epsilon*0.99^n, Qestim) 
    } else {
      Policy = EGPolicy(epsilon, Qestim)  
    }
  }
  return(list(Qestim, EpisodeReturn))
}




#epsilon / n
StartTime = Sys.time() 
FinalGreedyPolicy = matrix(data = NA, nrow = nb_runs, ncol = nb_states)
EpisodeReturn = matrix(data = NA, nrow = nb_runs, ncol = nepis)
ImprovedQ = array(data = NA, dim = c(nb_states, 4, nb_runs))
for(r in 1:nb_runs){
  run = QLearning(stepsize, Q_0, InitialPolicy, nepis, 0.4, r, 1)
  ImprovedQ = run[[1]]
  EpisodeReturn[r,] = run[[2]]
  FinalGreedyPolicy[r,] = max.col(ImprovedQ)
}
ImprovedQ = apply(ImprovedQ, 1:2, mean)
ImprovedPolicy_1 = apply(FinalGreedyPolicy, 2, getmode)
AvgEpisodeReturnQLearning_1 = colMeans(EpisodeReturn)
overNRunTime = Sys.time() - StartTime

#epsilon/ log(n+1)
StartTime = Sys.time() 
FinalGreedyPolicy = matrix(data = NA, nrow = nb_runs, ncol = nb_states)
EpisodeReturn = matrix(data = NA, nrow = nb_runs, ncol = nepis)
ImprovedQ = array(data = NA, dim = c(nb_states, 4, nb_runs))
for(r in 1:nb_runs){
  run = QLearning(stepsize, Q_0, InitialPolicy, nepis, 0.4, r, 2)
  ImprovedQ = run[[1]]
  EpisodeReturn[r,] = run[[2]]
  FinalGreedyPolicy[r,] = max.col(ImprovedQ)
}
ImprovedQ = apply(ImprovedQ, 1:2, mean)
ImprovedPolicy_2 = apply(FinalGreedyPolicy, 2, getmode)
AvgEpisodeReturnQLearning_2 = colMeans(EpisodeReturn)
overLogNRunTime = Sys.time() - StartTime

#epsilon * 0.99^n 
StartTime = Sys.time() 
FinalGreedyPolicy = matrix(data = NA, nrow = nb_runs, ncol = nb_states)
EpisodeReturn = matrix(data = NA, nrow = nb_runs, ncol = nepis)
ImprovedQ = array(data = NA, dim = c(nb_states, 4, nb_runs))
for(r in 1:nb_runs){
  run = QLearning(stepsize, Q_0, InitialPolicy, nepis, 0.4, r, 3)
  ImprovedQ = run[[1]]
  EpisodeReturn[r,] = run[[2]]
  FinalGreedyPolicy[r,] = max.col(ImprovedQ)
}
ImprovedQ = apply(ImprovedQ, 1:2, mean)
ImprovedPolicy_3 = apply(FinalGreedyPolicy, 2, getmode)
AvgEpisodeReturnQLearning_3 = colMeans(EpisodeReturn)
exponentialRunTime = Sys.time() - StartTime

#epsilon = constant 
StartTime = Sys.time() 
FinalGreedyPolicy = matrix(data = NA, nrow = nb_runs, ncol = nb_states)
EpisodeReturn = matrix(data = NA, nrow = nb_runs, ncol = nepis)
ImprovedQ = array(data = NA, dim = c(nb_states, 4, nb_runs))
for(r in 1:nb_runs){
  run = QLearning(stepsize, Q_0, InitialPolicy, nepis, 0.08, r, 4)
  ImprovedQ = run[[1]]
  EpisodeReturn[r,] = run[[2]]
  FinalGreedyPolicy[r,] = max.col(ImprovedQ)
}
ImprovedQ = apply(ImprovedQ, 1:2, mean)
ImprovedPolicy_4 = apply(FinalGreedyPolicy, 2, getmode)
AvgEpisodeReturnQLearning_4 = colMeans(EpisodeReturn)
constantRunTime = Sys.time() - StartTime


epsilon = NULL
plot(AvgEpisodeReturnQLearning_1, type = "l", col = "red", ylab = expression(G[0]), xlab = "Episode (n)", main = expression("50 Runs of QL with varying "*epsilon))
lines(AvgEpisodeReturnQLearning_2, type = "l", col = "blue")
lines(AvgEpisodeReturnQLearning_3, type = "l", col = "purple")
lines(AvgEpisodeReturnQLearning_4, type = "l", col = "green4")
legend(x=600, y=-50 , cex = 0.8,
       c("Optimal Return", "0.4/n", "0.4/log(n+1)", expression("  0.4*0.99"^"n"), "   0.08"),
       fill=c("orange", "red","blue","purple","green4"),
       border = c("orange", "black","black", "black","black"), bty = "y" )
lines(rep(3.85,1000), col = "orange")

print(overNRunTime / nb_runs) 
print(overLogNRunTime / nb_runs)
print(exponentialRunTime / nb_runs)
print(constantRunTime / nb_runs)

