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
#Set Initial Epsilon
epsilon = 0.4
#Set if epsilon should be decreasing within each episode.
decreasingEpsilon = 3
#Set Learning Step Size (Alpha)
stepsize = 0.1
#Initial Policy has prob 0.25 of chossing each action in all states.
InitialPolicy = matrix(data = 0.25, nrow = nb_states, ncol = 4)
#Initial estimate of state-action value function Q is identically 0. 
Q_0 = matrix(data = 0, nrow = nb_states, ncol = 4)


ApplySARSA_n = function(stepsize, nparam, nepis, initQestim, initPolicy, DF, epsilon, theseed, decreasingEpsilon){
  #n-step SARSA Algorithm
  Policy = initPolicy
  ImprovedQestim = initQestim
  Return = NULL
  for(n in 1:nepis){
    SAR = EpisodePolicy(n+theseed, Policy)
    L = ncol(SAR) - 1 
    #Update Qestim after each episode 
    Qestim = array(data = ImprovedQestim, dim =c(nb_states, 4, L))
    Qestim[,,2:nparam] = Qestim[,,1]
    for (t in 1:(L-nparam)){
      s_t = SAR[1,t] #S_t
      a_t = SAR[2,t] #A_t
      s_tn = SAR[1,t+nparam]  #S_t+n
      a_tn = SAR[2,t+nparam]  #A_t+n
      Tf = min(nparam, L-t) -1
      G = sum(SAR[3,t:(t+Tf)] * DF^(0:Tf)) + ifelse(nparam + t <= L,1,0)*(DF^nparam) * Qestim[s_tn,a_tn,t+nparam-1]
      #Updating Q(s,a) estimate 
      Qestim[,,t+nparam] = Qestim[,,t+nparam-1]
      Qestim[s_t,a_t,t+nparam] = Qestim[s_t,a_t,t+nparam-1] + stepsize*(G - Qestim[s_t,a_t,t+nparam-1])
    }
    #Improving Q-estimate and epsilon-greedy policy
    ImprovedQestim = Qestim[,,t+nparam]
    if (decreasingEpsilon == 1){
      Policy = EGPolicy(epsilon/n, ImprovedQestim)  
    } else if (decreasingEpsilon == 2){
      Policy = EGPolicy(epsilon/log(n+1), ImprovedQestim) 
    } else if (decreasingEpsilon == 3){
      Policy = EGPolicy(epsilon*0.99^n, ImprovedQestim) 
    } else {
      Policy = EGPolicy(epsilon, ImprovedQestim)  
    }
    #Episode returns
    Return[n] = sum(SAR[3,], na.rm = TRUE)
  }
  return(list(ImprovedQestim, Return))
}


#Generating 50 runs of 5-step SARSA
FinalGreedyPolicy = matrix(data = NA, nrow = nb_runs, ncol = nb_states)
EpisodeReturn = matrix(data = NA, nrow = nb_runs, ncol = nepis)
ImprovedQ = array(data = NA, dim = c(nb_states, 4, nb_runs))
for(r in 1:nb_runs){
  run = ApplySARSA_n(stepsize, 5, nepis, Q_0, InitialPolicy, 1, epsilon, r, 3)
  ImprovedQ[,,r] = run[[1]]
  EpisodeReturn[r,] = run[[2]]
  FinalGreedyPolicy[r,] = max.col(ImprovedQ[,,r])
}
#Taking averages
ImprovedQ = apply(ImprovedQ, 1:2, mean)
ImprovedPolicy = apply(FinalGreedyPolicy, 2, getmode)
AvgEpisodeReturnNStepSARSA5 = colMeans(EpisodeReturn)




#Generating 50 runs of 10-step SARSA
FinalGreedyPolicy = matrix(data = NA, nrow = nb_runs, ncol = nb_states)
EpisodeReturn = matrix(data = NA, nrow = nb_runs, ncol = nepis)
ImprovedQ = array(data = NA, dim = c(nb_states, 4, nb_runs))
for(r in 1:nb_runs){
  run = ApplySARSA_n(stepsize, 10, nepis, Q_0, InitialPolicy, 1, epsilon, r, 3)
  ImprovedQ[,,r] = run[[1]]
  EpisodeReturn[r,] = run[[2]]
  FinalGreedyPolicy[r,] = max.col(ImprovedQ[,,r])
}
#Taking averages
ImprovedQ = apply(ImprovedQ, 1:2, mean)
ImprovedPolicy = apply(FinalGreedyPolicy, 2, getmode)
AvgEpisodeReturnNStepSARSA10 = colMeans(EpisodeReturn)



#Generating 50 runs of 15-step SARSA
FinalGreedyPolicy = matrix(data = NA, nrow = nb_runs, ncol = nb_states)
EpisodeReturn = matrix(data = NA, nrow = nb_runs, ncol = nepis)
ImprovedQ = array(data = NA, dim = c(nb_states, 4, nb_runs))
for(r in 1:nb_runs){
  run = ApplySARSA_n(stepsize, 15, nepis, Q_0, InitialPolicy, 1, epsilon, r, 3)
  ImprovedQ[,,r] = run[[1]]
  EpisodeReturn[r,] = run[[2]]
  FinalGreedyPolicy[r,] = max.col(ImprovedQ[,,r])
}
#Taking averages
ImprovedQ = apply(ImprovedQ, 1:2, mean)
ImprovedPolicy = apply(FinalGreedyPolicy, 2, getmode)
AvgEpisodeReturnNStepSARSA15 = colMeans(EpisodeReturn)



#Plotting episode rewards (5STEPSARSA VS 10STEPSARSA vs 15STEPSARSA)
plot(1:nepis, y = AvgEpisodeReturnNStepSARSA5, type = "l", main="Average Return at Different N-Values of N-Step SARSA Across 50 Runs", xlab = "Episode", ylab = expression(G[0]), col = "red")
lines(x=1:nepis, y= AvgEpisodeReturnNStepSARSA10, type="l",col="blue")
lines(x=1:nepis, y= AvgEpisodeReturnNStepSARSA15, type="l",col="green")
lines(rep(3.85, nepis), col = "orange")
legend(x=500, y=-150 , cex = 0.8,
 c("5-Step SARSA","10-Step SARSA","15-Step SARSA","Optimal Return at State 1"),
 fill=c("red","blue","green","orange"), bty = "n" )

