# STAT 497 - Assignment 2
# Winter 2019
# Authors: 
# Matteo Esposito, William Ngo, Spyros Orfanos, Frédéric Siino

# Question 1 INCOMPLETE  ------------------------------------------
# 1.a)
getInteger = function(x,y,m){
  return(x+1+31*(y+1)+(31*101)*(m+1))
}

getX = function(n){
  if(n%%31 == 0){
    return (30)
  } else{
    return (n%%31-1)
  }
}

getY = function(n){
  valueOfX = getX(n)
  newN = (n-(valueOfX+1))/31
  if(newN%%101 == 0){
    return (100)
  } else{
    return (newN%%101-1)
  }
}

getM = function(n){
  valueOfX = getX(n)
  valueOfY = getY(n)
  newN = n-(valueOfX+1)-31*(valueOfY+1)
  return((newN/(31*101))-1)
}

getStates = function(n){
  return (array(data=c(getX(n),getY(n),getM(n))))
}
bowsersActionDamamge = function(){
  if(runif(1)>0.8){
    return (rbinom(1,size=5,prob=0.4))
  } else{
    return (rbinom(1,size=10,prob=0.7))
  }
}
stateTransition = function(state){
  if(state[1]<=5 && state[3]>0){
    return (c(ceiling(0.5*(30-state[1])-bowsersActionDamamge()),state[2],state[3]-1))
  } else{
    return (c(state[1]-bowsersActionDamamge(),state[2]-5,state[3]))
  }
}
rewardObtained = function(state){
  if(state[2]==0){
    return (1)
  } else{
    return(0)
  }
}


PolicyStateValueFunction = array(NA,dim=c(31,101,4))
for(z in 1:4){
  for(x in 1:31){
    for(y in 1:101){
      if(x==1 || y==1){
        PolicyStateValueFunction[x,y,z] =0
      }
    }
  }
}

# Question 2 ------------------------------------------
# 2.a)
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

SASRp = matrix(data = cbind(r1,r2,r3,r4,r5,r6,r7) ,nrow = 7, ncol = 5, byrow = TRUE)
colnames(SASRp) = c("s", "a", "s'", "r","  p(s',a',r|s,a)")
SASRp

# 2.b)
CalculatePolicyValueFunction = function(Policy, SASRp, DF){
  #Calculating the b vector
  b = NULL 
  for (i in 1:2){
    b[i] = sum(SASRp[SASRp[,"s"] %in% i & SASRp[,"a"] %in% Policy[i], 4] * SASRp[SASRp[,"s"] %in% i & SASRp[,"a"] %in% Policy[i], 5])
  }
  #Calculating the M matrix
  M = matrix(data = NA, nrow = 2, ncol = 2)
  for(i in 1:2){
    for (j in 1:2){
      M[i,j] =  DF * sum(SASRp[SASRp[,"s"] %in% i & SASRp[,"a"] %in% Policy[i] & SASRp[,"s'"] %in% j, 5])
    }
  }
  #Solving for the policy's value function
  PolicyVF = as.vector(solve(diag(2) - M) %*% b)
  return(PolicyVF)
}

#Matrix containing all possible policy combinations
Policies = cbind( c(rep(1,3), rep(2,3)), c(1:3,1:3) )
VFAllPolicies = matrix(data = NA, nrow = 6, ncol = 2)

#Calculating value function for all 6 policies
for (p in 1:6){
  VFAllPolicies[p,] = CalculatePolicyValueFunction(Policy = Policies[p,], SASRp = SASRp, DF = 0.9)
}
colnames(VFAllPolicies) = c("State 1","State 2")
VFAllPolicies
#Policy 3 is optimal as it has the highest state-value
#Finding the optimal policy (double check)
optimal_s1 = Policies[which.max(VFAllPolicies[,1]),1]
optimal_s2 = Policies[which.max(VFAllPolicies[,2]),2]
c(optimal_s1, optimal_s2)


# 2.c)
PolicyEvaluation = function(StartVF,Policy,SASRp,DF,niter){
  VFEstimateNext = StartVF
  for(i in 1:niter){
    temp = rep(0,2)
    for(j in 1:length(StartVF)){
      for(k in 1:7){
        if(SASRp[k,1]==j && SASRp[k,2]==Policy[j]){
          temp[j] = temp[j] + SASRp[k,5]*(SASRp[k,4]+DF*VFEstimateNext[SASRp[k,3]])
        }
      }
    }
    VFEstimateNext = temp
  }
  return (VFEstimateNext)
}
PolicyEvaluation(c(3,2),c(1,1),SASRp,0.9,1)

# 2.d)
PolicyImprovement = function(VFEstim, SASRp, DF){
  #Calculating state-action value function based on estimate
  Q = matrix(data = NA, nrow = 2, ncol = 3)
  for(i in 1:2){
    for (j in 1:3){
      SASRp_temp = matrix(data = SASRp[SASRp[,"s"] %in% i & SASRp[,"a"] %in% j,], ncol = 5)
      Q[i,j] = sum( (SASRp_temp[,4] + DF*VFEstim[SASRp_temp[,3]]) * SASRp_temp[,5] )
    }
  }
  #Finding the optimal policy
  PolicyEstimate = NULL
  PolicyEstimate[1] = which.max(Q[1,])
  PolicyEstimate[2] = which.max(Q[2,])
  return(PolicyEstimate)
}
PolicyImprovement(VFEstim = c(3,2), SASRp = SASRp, DF = 0.9)

# 2.e)
for(i in 1:20){
  if(i==1){
    valueFunctionEstimates = PolicyEvaluation(c(1,0),c(2,2),SASRp,0.9,10)
  } else{
    valueFunctionEstimates = PolicyEvaluation(valueFunctionEstimates,optimalPolicyEstimates,SASRp,0.9,10)
  }
  optimalPolicyEstimates = PolicyImprovement(valueFunctionEstimates,SASRp,0.9)
}
valueFunctionEstimates
optimalPolicyEstimates

# 2.f)
ValueIteration = function(StartVF, SASRp, DF, niter){
  VFEstimateCurrent = StartVF
  SA_VFEstimateNext = matrix(data = NA, nrow = 2, ncol = 3)
  Policy = rep(0,2)
  for (n in 1:niter){
    for (i in 1:2){
      for (j in 1:3){
        #Calculating state-action value function estimate
        SASRp_temp = matrix(data = SASRp[SASRp[,"s"] %in% i & SASRp[,"a"] %in% j,], ncol = 5)
        SA_VFEstimateNext[i,j] = sum( (SASRp_temp[,4] + DF*VFEstimateCurrent[SASRp_temp[,3]]) * SASRp_temp[,5] )
      }
    }
    for (s in 1:2){
      #Calculating optimal policy and corresponding value function
      Policy[s] = which.max(SA_VFEstimateNext[s,])
      VFEstimateCurrent[s] = max(SA_VFEstimateNext[s,], na.rm = TRUE)
    }
  }
  ListVFPolicy = list(VFEstimateCurrent, Policy)
  return(ListVFPolicy)
}
#Applying value iteration with 2,5,25, and 500 iterations
niter_vec = c(2,5,25,500)
Answer = NULL
for(i in 1:4){
  Answer[[i]] = ValueIteration(StartVF = c(0,0), SASRp = SASRp, DF = 0.9, niter = niter_vec[i])
}
Answer


# Question 3 ------------------------------------------
# 3.a)
#We will assumume that card value 10 is equivalent as any 10, Jack,Queen,King and card value 11 is equal to ace
SimulateBlackJackEpisodeStick20 = function(theseed){
  set.seed(theseed)
  #First card
  keepTrackPlayers = c(obtainACard())
  playersCards = evalAce(keepTrackPlayers)
  playersTotal = c(sum(playersCards))
  playersequenceOfUsable1s = c(ifelse(11 %in% playersCards,1,0))
  keepTrackDealers = c(obtainACard())
  dealersCards = evalAce(keepTrackDealers)
  dealersTotal = c(sum(dealersCards))
  #Second card
  keepTrackPlayers = c(keepTrackPlayers,obtainACard())
  playersCards = evalAce(c(playersCards,keepTrackPlayers[length(keepTrackPlayers)]))
  playersTotal = c(playersTotal,sum(playersCards))
  playersequenceOfUsable1s = c(playersequenceOfUsable1s,ifelse(11 %in% playersCards,1,0))
  keepTrackDealers = c(keepTrackDealers,obtainACard())
  dealersCards = evalAce(c(dealersCards,keepTrackDealers[length(keepTrackDealers)]))
  dealersTotal = c(dealersTotal,sum(dealersCards))
  
  #Determine if the game should end right away
  if(playersTotal[length(playersTotal)]==21 && dealersTotal[length(dealersTotal)] != 21){
    return(list(1,keepTrackPlayers,playersTotal,playersequenceOfUsable1s,keepTrackDealers,dealersTotal))
  }
  if(playersTotal[length(playersTotal)]==21 && dealersTotal[length(dealersTotal)] == 21){
    return(list(0,keepTrackPlayers,playersTotal,playersequenceOfUsable1s,keepTrackDealers,dealersTotal))
  }
  
  #Player Playing
  playersChoice = "stop"
  if(playersTotal[length(playersTotal)]<20)
    playersChoice = "hit"
  while(playersChoice=="hit"){
    keepTrackPlayers = c(keepTrackPlayers,obtainACard())
    playersCards = evalAce(c(playersCards,keepTrackPlayers[length(keepTrackPlayers)]))
    playersTotal = c(playersTotal,sum(playersCards))
    playersequenceOfUsable1s = c(playersequenceOfUsable1s,ifelse(11 %in% playersCards,1,0))
    if(playersTotal[length(playersTotal)]==20 || playersTotal[length(playersTotal)] ==21){
      playersChoice = "stop"
    }
    if(playersTotal[length(playersTotal)]>21){
      return(list(-1,keepTrackPlayers,playersTotal,playersequenceOfUsable1s,keepTrackDealers,dealersTotal))
    }
  }
  
  #Dealer Playing
  dealersChoice="hit"
  while(dealersChoice=="hit"){
    if(dealersTotal[length(dealersTotal)]<17){
      keepTrackDealers = c(keepTrackDealers,obtainACard())
      dealersCards = evalAce(c(dealersCards,keepTrackDealers[length(keepTrackDealers)]))
      dealersTotal = c(dealersTotal,sum(dealersCards))
    } else{
      dealersChoice="stop"
    }
  }
  
  if(dealersTotal[length(dealersTotal)]>21){
    return(list(1,keepTrackPlayers,playersTotal,playersequenceOfUsable1s,keepTrackDealers,dealersTotal))
  } else if(dealersTotal[length(dealersTotal)] == playersTotal[length(playersTotal)]){
    return(list(0,keepTrackPlayers,playersTotal,playersequenceOfUsable1s,keepTrackDealers,dealersTotal))
  } else if(dealersTotal[length(dealersTotal)] > playersTotal[length(playersTotal)]){
    return(list(-1,keepTrackPlayers,playersTotal,playersequenceOfUsable1s,keepTrackDealers,dealersTotal))
  } else if(dealersTotal[length(dealersTotal)] < playersTotal[length(playersTotal)]){
    return(list(1,keepTrackPlayers,playersTotal,playersequenceOfUsable1s,keepTrackDealers,dealersTotal))
  } 
}

evalAce = function(hand){
  #Function evaluates if ace should be valued as 11 or 1
  total = sum(hand)
  temp = hand
  for(i in hand){
    if(i==11 && total>21){
      temp[temp==11] =1
      return (temp)
    }
  }
  return (temp)
}

obtainACard = function(){
  #Returns a number from 2-11
  temp = sample(1:13,size=1)
  if(temp>=10 && temp<=13)
    temp=10
  if(temp==1)
    temp=11
  return (temp)
}


