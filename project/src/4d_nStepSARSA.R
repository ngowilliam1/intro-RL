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


#Generating 100 runs of n-step SARSA
StartTime = Sys.time() 
FinalGreedyPolicy = matrix(data = NA, nrow = nb_runs, ncol = nb_states)
EpisodeReturn = matrix(data = NA, nrow = nb_runs, ncol = nepis)
ImprovedQ = array(data = NA, dim = c(nb_states, 4, nb_runs))
for(r in 1:nb_runs){
  run = ApplySARSA_n(stepsize, nparam, nepis, Q_0, InitialPolicy, 1, epsilon, r, decreasingEpsilon)
  ImprovedQ[,,r] = run[[1]]
  EpisodeReturn[r,] = run[[2]]
  FinalGreedyPolicy[r,] = max.col(ImprovedQ[,,r])
}
#Taking averages
ImprovedQ = apply(ImprovedQ, 1:2, mean)
ImprovedPolicy = apply(FinalGreedyPolicy, 2, getmode)
AvgEpisodeReturnNStepSARSA = colMeans(EpisodeReturn)
nStepTime = Sys.time() - StartTime

#Drawing optimal actions 
xy_a_nStep = cbind(valid_positions, ImprovedPolicy)
draw_board(paste(nparam,"-Step SARSA,",nb_runs,"Runs"), xy_a_nStep)
