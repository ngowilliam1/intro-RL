#STAT 497: Reinforcement Learning. Assignment III. 
#Authors: Spyros Orfanos, William Ngo, Matteo Esposito. 

#-------------------------- Question 1 ---------------------------
#1(a)
alpha = 0.6
beta = 0.8
r_srch = 2
r_wait = 1

r1 = c(1,1,1,r_srch, alpha)
r2 = c(1,1,2,r_srch, 1-alpha)
r3 = c(2,1,1,-3, 1-beta)
r4 = c(2,1,2,r_srch, beta)
r5 = c(1,2,1,r_wait, 1)
r6 = c(2,2,2,r_wait, 1)
r7 = c(2,3,1,0,1)
r8 = c(1,3,1,0,1)

SASRp = matrix(data = cbind(r1,r2,r3,r4,r5,r6,r7,r8), nrow = 8, ncol = 5, byrow = TRUE)
SASRp = SASRp[order(SASRp[,1]),]
colnames(SASRp) = c("s","a","s'","r","p")

#Calculating V(s)
CalculatePolicyValueFunction = function(Policy, SASRp, DF){
  #Calculating the b vector
  b = c(0,0)
  for (ss in 1:2){
    for (aa in 1:3){
      b[ss] = b[ss] + sum( SASRp[SASRp[,"s"] %in% ss & SASRp[,"a"] %in% aa,"r"] * SASRp[SASRp[,"s"] %in% ss & SASRp[,"a"] %in% aa,"p"] * Policy[ss,aa] )
    } 
  }
  #Calculating the M matrix
  M = matrix(data = 0, nrow = 2, ncol = 2)
  for(ss in 1:2){
    for(sp in 1:2){
      for(aa in 1:3){
        M[ss,sp] =   M[ss,sp] + DF * sum( SASRp[SASRp[,"s"] %in% ss & SASRp[,"a"] %in% aa & SASRp[,"s'"] %in% sp, "p"] * Policy[ss,aa] )
      }
    }
  }
  #Solving for the policy's value function
  PolicyVF = as.vector(solve(diag(2) - M) %*% b)
  return(PolicyVF)
}

#Calculating Q(s,a) = sum_{all r,s'} of p*(r + DF*v(s'))
CalculatePolicyActionValueFunction = function(Policy, SASRp, DF){
  PolicyAVF =  matrix(data = NA, nrow = 2, ncol = 3)
  V = CalculatePolicyValueFunction(Policy, SASRp, DF)
  for(ss in 1:2){
    for(aa in 1:3){
      sa = SASRp[,"s"] %in% ss & SASRp[,"a"] %in% aa  #state-action pair
      PolicyAVF[ss,aa] = sum( (SASRp[sa,"r"] + DF*V[SASRp[sa,"s'"]]) * SASRp[sa,"p"] )
    }
  }
  return(PolicyAVF)
}
Policy = matrix( data = c(0.7,0.2,0.1,0.1,0.7,0.2), nrow = 2, byrow = TRUE)
PolicyAVF = CalculatePolicyActionValueFunction(Policy, SASRp, DF = 0.9)
PolicyAVF

#1(b)
#See sample() documentation. Error arises when as sample(X, 1) is interpreted as sample from {1,...,X} instead of {X}.
resample = function(x, ...) x[sample(length(x), ...)]

#Given state (ss), simulate next action (aa), then simulate next state (sp), and finally determine the reward (rr) which is a function of ss,aa,sp.
SimulateEpsiodeCanRecycler = function(Policy, SASRp, Eplength, initstate, theseed){
  set.seed(theseed)
  EpisodeOutcomes = matrix(data = NA, nrow = 3, ncol = Eplength)
  ss = initstate
  for (t in 1:Eplength){
    aa = sample(1:3, 1, prob = Policy[ss,])
    sp = resample(SASRp[SASRp[,"s"] %in% ss & SASRp[,"a"] %in% aa, "s'"], 1, prob = SASRp[SASRp[,"s"] %in% ss & SASRp[,"a"] %in% aa, "p"])
    rr = SASRp[SASRp[,"s"] %in% ss & SASRp[,"a"] %in% aa & SASRp[,"s'"] %in% sp, "r"]
    EpisodeOutcomes[,t] = c(ss, aa, rr)
    ss = sp
  }
  return(EpisodeOutcomes)
}

#1(c)
#Function that gives the corresponding row of Qestimmat for a given state-action pair. 
which_row = function(EpisodeOutcomes,t){
  if(all(EpisodeOutcomes[1:2,t] == c(1,1))){return(1)}
  if(all(EpisodeOutcomes[1:2,t] == c(1,2))){return(2)}
  if(all(EpisodeOutcomes[1:2,t] == c(1,3))){return(3)}
  if(all(EpisodeOutcomes[1:2,t] == c(2,1))){return(4)}
  if(all(EpisodeOutcomes[1:2,t] == c(2,2))){return(5)}
  if(all(EpisodeOutcomes[1:2,t] == c(2,3))){return(6)}
}

ApplySARSA = function(stepsize, nparam, initQestim, EpisodeOutcomes, DF){
  Qestimmat = matrix(data = NA, nrow = 6, ncol = ncol(EpisodeOutcomes))
  Qestimmat[,1:nparam] = initQestim
  L = ncol(EpisodeOutcomes) - nparam
  for (t in 1:L){
    sa_t = which_row(EpisodeOutcomes, t) #(S_t, A_t)
    sa_tn = which_row(EpisodeOutcomes, t+nparam)  #(S_t+n, A_t+n)
    G = sum(EpisodeOutcomes[3,t:(t+nparam-1)] *DF^(0:(nparam-1))) + Qestimmat[sa_tn,t+nparam-1] * DF^nparam 
    #Updating Q(s,a) estimate 
    Qestimmat[,t+nparam] = Qestimmat[,t+nparam-1]
    Qestimmat[sa_t,t+nparam] = Qestimmat[sa_t,t+nparam-1] + stepsize*(G - Qestimmat[sa_t,t+nparam-1]) 
  }
  return(Qestimmat)
}

#1(d)
Episode = SimulateEpsiodeCanRecycler(Policy, SASRp, 10000, 1, 497)
one_step = ApplySARSA(0.01, 1, rep(0,6), Episode, 0.9)
five_step = ApplySARSA(0.01, 5, rep(0,6), Episode, 0.9)
twenty_step = ApplySARSA(0.01, 20, rep(0,6), Episode, 0.9)
true = as.vector(t(PolicyAVF))

RMSEmat = matrix(data = NA, nrow = 3, ncol = 10000)
RMSEmat[1,] = sqrt(colSums((one_step - true)^2))
RMSEmat[2,] = sqrt(colSums((five_step - true)^2))
RMSEmat[3,] = sqrt(colSums((twenty_step - true)^2))

#very long title  
plot(1:10000, RMSEmat[1,], type = "l", col = "blue", xlab = "Time Step", ylab = "RMSE", main = "RMSE of n-step SARSA with step size 0.01, under policy pi", ylim = c(0,28))
lines(RMSEmat[2,], type = "l", col = "red")
lines(RMSEmat[3,], type = "l", col = "green")
legend("right", legend = c("n=1", "n=5", "n=20"), col = c("blue", "red", "green"), lty=1:1, cex=0.9, lwd =1)

#1(e)
ApplyQLearning = function(stepsize, initQestim, EpisodeOutcomes, DF){
  Qestim = initQestim
  E = EpisodeOutcomes
  for (tt in 1:(ncol(E)-1)){
    Qestim[E[1,tt], E[2,tt]] = Qestim[E[1,tt], E[2,tt]] + stepsize*( E[3,tt] + DF*max(Qestim[E[1,tt+1], ]) -  Qestim[E[1,tt],E[2,tt]] ) 
  }
  return(Qestim)
}

Episode_e = SimulateEpsiodeCanRecycler(Policy, SASRp, 25000, 1, 91)
q = ApplyQLearning(0.01, matrix(data = 0, nrow = 2, ncol = 3), Episode_e, 0.9)
q #Estimated
CalculatePolicyActionValueFunction(matrix(c(1,0,0,0,0,1), nrow = 2, byrow = TRUE), SASRp, 0.9) #True 



#-------------------------- Question 2 ---------------------------
#2(a)
SASR = matrix(data = NA, nrow = 6, ncol = 5)
colnames(SASR) = c("s", "a", "s'", "r", "p")
SASR[,1] = c(1,1,2,2,3,3)
SASR[,2] = c(1,-1, 1, -1, 1, -1)
SASR[,3] = c(2,1,1,3,4,2) 
SASR[,4] = c(-1,-1,-1,-1,-1,-1)
SASR

#See sample() documentation. Error arises as sample(X, 1) is interpreted as sample from {1,...,X} instead of {X}.
resample = function(x, ...) x[sample(length(x), ...)]

SimulCorridorEpisodeFixedPolicy = function(pright, theseed){
  set.seed(theseed)
  stateseq = 1
  actionseq = NULL
  #Adding the transition probabilities to SASR.
  SASR[SASR[,"a"] %in% 1,"p"] = pright
  SASR[SASR[,"a"] %in% -1,"p"] = 1 - pright
  #Run continues until state 4 is reached. 
  while(stateseq[length(stateseq)] != 4){
    actionseq[length(actionseq)+1] = resample(SASR[SASR[,"s"] %in% stateseq[length(stateseq)],"a"], 1, prob = SASR[SASR[,"s"] %in% stateseq[length(stateseq)],"p"])
    stateseq[length(stateseq)+1] = SASR[SASR[,"s"] %in% stateseq[length(stateseq)] & SASR[,"a"] %in% actionseq[length(actionseq)], "s'"]
  }
  Result = list(stateseq, actionseq)
  names(Result) = c("stateseq", "actionseq")
  return(Result)
}

right_95 = NULL
right_05 = NULL
for(i in 1:20000){
  right_95[i] = -1*length(SimulCorridorEpisodeFixedPolicy(.95,i)$actionseq)
  right_05[i] = -1*length(SimulCorridorEpisodeFixedPolicy(.05,i)$actionseq)
}
mean(right_95) #-44.11255
mean(right_05) #-81.8691


#2(b)
#See report. 

#2(c)
OneRunCorridor = function(inittheta, alphasteptheta, nepis, theseed){
  ProbRightEachEpisode = exp(inittheta[1])/sum(exp(inittheta))
  RewardEachEpisode = NULL
  theta = inittheta
  for (n in 1:nepis){
    Episode = SimulCorridorEpisodeFixedPolicy(ProbRightEachEpisode[n], theseed*n)
    Tf = length(Episode$actionseq) 
    for (t in 1:Tf){
      G = -1 * (Tf - t + 1) 
      theta[1] = theta[1] + alphasteptheta * G * (ifelse(Episode$actionseq[t] ==  1,1,0) - (ProbRightEachEpisode[n])   )
      theta[2] = theta[2] + alphasteptheta * G * (ifelse(Episode$actionseq[t] == -1,1,0) - (1-ProbRightEachEpisode[n]) )
    }
    RewardEachEpisode[n] = -1*Tf
    ProbRightEachEpisode[n+1] = max(min(exp(theta[1])/sum(exp(theta)),0.99),0.01)
  }
  Result =list(RewardEachEpisode, ProbRightEachEpisode[-1])
  return(Result)
}

#d)
returnmat = array(NA,dim=c(100,1000,3))
probrightmat = array(NA,dim=c(100,1000,3))
for (i in 1:100){
  largeAlpha=OneRunCorridor(inittheta=c(log(1/0.95-1),0),alphasteptheta=2^(-12),nepis=1000,theseed=i)
  MediumAlpha=OneRunCorridor(inittheta=c(log(1/0.95-1),0),alphasteptheta=2^(-13),nepis=1000,theseed=i)
  lowAlpha=OneRunCorridor(inittheta=c(log(1/0.95-1),0),alphasteptheta=2^(-14),nepis=1000,theseed=i)
  returnmat[i,,1]=largeAlpha[[1]]
  returnmat[i,,2]=MediumAlpha[[1]]
  returnmat[i,,3]=lowAlpha[[1]]
  probrightmat[i,,1]=largeAlpha[[2]]
  probrightmat[i,,2]=MediumAlpha[[2]]
  probrightmat[i,,3]=lowAlpha[[2]]
}


#Plotting average reward accross 100 runs 
plot(x = 1:1000, y = colMeans(returnmat[,,1]), type = "l", col = "blue", xlab = "Episode", ylab = expression(G[0]), ylim = c(-100,0))
lines(colMeans(returnmat[,,2]), type = "l", col = "red")
lines(colMeans(returnmat[,,3]), type = "l", col = "green")
lines( rep(-11.6,1000), type = "l", lty = 2, col = "grey")
legend(x = -10, y = -3, legend = expression({v["*"](s[0])}=={-11.6}), text.col = "grey30", bty = "n", cex = 0.7)
legend("bottomright", legend = expression({alpha[theta]}=={2^-12},{alpha[theta]}=={2^-13}, {alpha[theta]}=={2^-14}), text.col = c("blue","red","green"))

#Plotting probability of selecting 'Right'
plot(x = 1:1000, y = colMeans(probrightmat[,,1]), type = "l", col = "blue", xlab = "Episode", ylab = "Probability of Selecting 'Right'", ylim = c(0,.82))
lines(colMeans(probrightmat[,,2]), type = "l", col = "red")
lines(colMeans(probrightmat[,,3], na.rm = T), type = "l", col = "green")
lines( rep(0.59,1000), type = "l", lty = 2, col = "grey")
legend(x = -10, y = 0.67, legend = expression({"Optimal Prob"}=={0.59}), text.col = "grey30", bty = "n", cex = 0.7)
legend("topright", legend = expression({alpha[theta]}=={2^-12},{alpha[theta]}=={2^-13}, {alpha[theta]}=={2^-14}), text.col = c("blue","red","green"), cex = 0.75)


#2(e)
OneRunCorridorBaseline = function(inittheta,initw, alphasteptheta,alphastepw, nepis, theseed){
  ProbRightEachEpisode = exp(inittheta[1]) / sum(exp(inittheta))
  RewardEachEpisode = NA
  theta = inittheta
  w = initw
  for (n in 1:nepis){
    Episode = SimulCorridorEpisodeFixedPolicy(ProbRightEachEpisode[n], theseed*n)
    Tf = length(Episode$actionseq)
    for (t in 1:Tf){
      G = -1 * (Tf - t + 1) 
      delta = G - w[Episode$stateseq[t]]
      w[Episode$stateseq[t]] = w[Episode$stateseq[t]] + alphastepw * (G - w[Episode$stateseq[t]])
      theta[1] = theta[1] + alphasteptheta *delta* (ifelse(Episode$actionseq[t] ==  1,1,0) - ProbRightEachEpisode[n])
      theta[2] = theta[2] + alphasteptheta *delta* (ifelse(Episode$actionseq[t] == -1,1,0) - (1-ProbRightEachEpisode[n]))
    }
    RewardEachEpisode[n] = -1*Tf
    ProbRightEachEpisode[n+1] = max(min(exp(theta[1])/sum(exp(theta)),0.99),0.01)
  }
  Result =list(RewardEachEpisode, ProbRightEachEpisode[-1])
  return(Result)
}

#2(f)
initialw = c(0,0,0,0)
returnmatBaseline = array(NA,dim=c(100,1000,2))
for (i in 1:100){
  largeAlpha=OneRunCorridorBaseline(inittheta=c(log(1/0.95-1),0),initw=initialw,alphasteptheta=2^(-9),alphastepw=2^(-6),nepis=1000,theseed=i)
  lowAlpha=OneRunCorridorBaseline(inittheta=c(log(1/0.95-1),0),initw=initialw,alphasteptheta=2^(-12),alphastepw=2^(-6),nepis=1000,theseed=i)
  returnmatBaseline[i,,1]=largeAlpha[[1]]
  returnmatBaseline[i,,2]=lowAlpha[[1]]
}


plot(x = 1:1000, y = colMeans(returnmatBaseline[,,1]) , type = "l", col = "green", xlab = "Episode", ylab = expression(G[0]), ylim = c(-100,-8))
lines(colMeans(returnmatBaseline[,,2]) , type = "l", col = "blue")
lines(colMeans(returnmat[,,1]) , type = "l", col = "red")
lines( rep(-11.6,1000), type = "l", lty = 2, col = "grey30")
legend("bottomright", legend = expression(paste(alpha[theta]==2^-9,", ",alpha[w]==2^-6), paste(alpha[theta]==2^-12,", ",alpha[w]==2^-6), paste({alpha[theta]}=={2^-12}, ", No Baseline"))
  , text.col = c("green","blue","red"))
legend(x = -10, y = -2, legend = expression({v["*"](s[0])}=={-11.6}), text.col = "grey30", bty = "n", cex = 0.75)

