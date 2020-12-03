ApplySARSA = function(stepsize, nepis, initQestim, initPolicy, DF, epsilon, theseed, decreasingEpsilon){
  #SARSA Algorithm
  Policy = initPolicy
  Qestim = initQestim
  EpisodeReturn = NULL
  for(n in 1:nepis){
    SAR = EpisodePolicy(n+theseed, Policy)
    EpisodeReturn[n] = sum(SAR[3,], na.rm = TRUE) 
    for (t in 1:(ncol(SAR) - 1)){ 
      Qestim[SAR[1,t],SAR[2,t]] = Qestim[SAR[1,t],SAR[2,t]] +stepsize*(SAR[3,t]+DF*Qestim[SAR[1,t+1],SAR[2,t+1]] -Qestim[SAR[1,t],SAR[2,t]])
    }
    #Improved epsilon-greedy policy
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
  run = ApplySARSA(stepsize, nepis, Q_0,InitialPolicy,1, epsilon, r, decreasingEpsilon)
  ImprovedQ[,,r] = run[[1]]
  EpisodeReturn[r,] = run[[2]]
  FinalGreedyPolicy[r,] = max.col(ImprovedQ[,,r])
}
#Taking averages
ImprovedQ = apply(ImprovedQ, 1:2, mean)
ImprovedPolicy = apply(FinalGreedyPolicy, 2, getmode)
AvgEpisodeReturnSARSA = colMeans(EpisodeReturn)
SARSATime = Sys.time() - StartTime

#Drawing optimal actions 
xy_a_SARSA = cbind(valid_positions, ImprovedPolicy)
draw_board(paste("SARSA",nb_runs,"Runs"), xy_a_SARSA)

