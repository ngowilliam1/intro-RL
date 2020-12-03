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


#Generating 100 runs of Q-Learning 
StartTime = Sys.time() 
FinalGreedyPolicy = matrix(data = NA, nrow = nb_runs, ncol = nb_states)
EpisodeReturn = matrix(data = NA, nrow = nb_runs, ncol = nepis)
ImprovedQ = array(data = NA, dim = c(nb_states, 4, nb_runs))
for(r in 1:nb_runs){
  run = QLearning(stepsize, Q_0, InitialPolicy, nepis, epsilon, r, decreasingEpsilon)
  ImprovedQ[,,r] = run[[1]]
  EpisodeReturn[r,] = run[[2]]
  FinalGreedyPolicy[r,] = max.col(ImprovedQ[,,r])
}
#Taking averages
ImprovedPolicy = apply(FinalGreedyPolicy, 2, getmode)
AvgEpisodeReturnQLearning = colMeans(EpisodeReturn)
ImprovedQ = apply(ImprovedQ, 1:2, mean)
QLearningTime = Sys.time() - StartTime

#Drawing optimal actions 
xy_a_QL = cbind(valid_positions, ImprovedPolicy)
draw_board(paste("Q-Learning",nb_runs,"Runs"), xy_a_QL)
