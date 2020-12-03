# STAT 497 - Assignment 2
# Winter 2019
# Authors: Matteo Esposito, William Ngo, Spyros Orfanos, Frederic Siino

# Preliminary ------------------------------------------

install.packages("plot3D")
library(plot3D)
options(max.print=1000000) # For large console output

# Question 1 ------------------------------------------
# (a) ------------------------------------------

M_HP = 0:30
B_HP = 0:100
n_msh = 0:3

#The entire state space
States = expand.grid(M_HP, B_HP, n_msh)
names(States) = c("M_HP", "B_HP", "Nb_Msh")

#Pr{Bowser's damage = j} for j = 0,1,..,10 has following pmf:
B_attack_prob = 0.8*c(dbinom(0:5, 5, 0.4), rep(0,5)) + 0.2*dbinom(0:10, 10, 0.7)

#The policy state-value function. VF of terminal states are set to 0.
M_HP_txt = paste("M_HP:" , M_HP, sep = "", collapse = NULL)
B_HP_txt = paste("B_HP:" , B_HP, sep = "", collapse = NULL)
n_msh_txt = paste("Remaining Mushrooms: ", n_msh, sep = "", collapse = NULL)
PolicyStateValueFunction = array(data = NA, dim = c(31,101,4), dimnames = list(M_HP_txt, B_HP_txt,n_msh_txt))
PolicyStateValueFunction[,1,] = 0
PolicyStateValueFunction[1,,] = 0

#Calculating a matrix containing: current state, action taken under policy pi, future state, 4-argument p, and reward.
policy_pi = function(marioHP, bowserHP, numMushroom){
  if (numMushroom == 0 || marioHP > 5){
    Action = 0            #Mario attacks
    bowserHP_Next = max(bowserHP - 5, 0)
    marioHP_Next = cbind(marioHP - 0:10, B_attack_prob)
    #If Mario's HP falls below zero, set it to 0. Outcome is unaffected as the game ends immidiately (Mario wins if Bowser's HP is also 0).
    if( min(marioHP_Next) < 0){
      trunc = which(marioHP_Next[,1] == 0)
      marioHP_Next = rbind(marioHP_Next[1:(trunc-1),], c(0,sum(marioHP_Next[trunc:11,2])))
    }
    
  } else if (numMushroom > 0 && marioHP <=5) {  
    Action = 1            #Mario eats a mushroom 
    bowserHP_Next = bowserHP
    numMushroom = numMushroom - 1
    marioHP_Next = cbind(marioHP + ceiling(0.5*(30-marioHP)) - 0:10, B_attack_prob)
  }
  
  l = nrow(marioHP_Next)
  Reward = rep(ifelse(bowserHP_Next <= 0, 1, 0), l)
  temp = matrix(data = rep(c(marioHP,bowserHP,numMushroom,Action), l), ncol = 4, byrow  = TRUE)
  SASRp = cbind(temp, marioHP_Next[,1], rep(bowserHP_Next,l), rep(numMushroom,l), marioHP_Next[,2], Reward)
  colnames(SASRp) = c("xstart", "ystart", "zstart", "Action", "xprime", "yprime", "zprime", "prob", "reward")
  
  #If Mario's starting HP is 0, the game is over. 
  if(marioHP == 0){
    SASRp = "GAME OVER. TRY AGAIN?"
  }
  
  return(SASRp)
}

#Calculating the state-action value for all the states in the state space. 
for (ms in 1:4){
  for (m in 2:31){
    for(b in 2:101){
      temp_mat = policy_pi(marioHP= M_HP[m], bowserHP= B_HP[b], numMushroom= n_msh[ms])  
      SVF_prime = NULL
      for (i in 1:nrow(temp_mat)){
        SVF_prime[i] = PolicyStateValueFunction[temp_mat[i,"xprime"]+1, temp_mat[i,"yprime"] +1, temp_mat[i,"zprime"]+1]
      }
      temp_mat = cbind(temp_mat, SVF_prime)
      PolicyStateValueFunction[m, b, ms] = sum((temp_mat[,"SVF_prime"] + temp_mat[,"reward"])*temp_mat[,"prob"])
    }
  }
}
temp_mat
round(PolicyStateValueFunction[,,], 3)

# (b) ------------------------------------------
any_action = function(marioHP, bowserHP, numMushroom){
  SASRp_1 = NULL
  
  #Transition probabilities if Mario attacks Bowser
  Action = 0
  bowserHP_Next = max(bowserHP - 5, 0)
  marioHP_Next = cbind(marioHP - 0:10, B_attack_prob)
  
  if(min(marioHP_Next) < 0){
    trunc = which(marioHP_Next[,1] == 0)
    marioHP_Next = rbind(marioHP_Next[1:(trunc-1),], c(0,sum(marioHP_Next[trunc:11,2])))
  }
  
  l = nrow(marioHP_Next)
  Reward = rep(ifelse(bowserHP_Next <= 0, 1, 0), l)
  temp = matrix(data = rep(c(marioHP,bowserHP,numMushroom,Action), l), ncol = 4, byrow  = TRUE)
  SASRp_0 = cbind(temp, marioHP_Next[,1], rep(bowserHP_Next,l), rep(numMushroom,l), marioHP_Next[,2], Reward)
  
  #Transition probabilities if Mario eats a mushroom (provided he can, of course)
  if(numMushroom > 0){
    #Note: After eating a mushroom, game cannot end in subsequent stage (Mario HP > 15 so can't die immediately, and Bowser's HP does not change)
    Action = 1
    bowserHP_Next = bowserHP
    numMushroom = numMushroom - 1
    marioHP_Next = cbind(marioHP + ceiling(0.5*(30-marioHP)) - 0:10, B_attack_prob)
    Reward = rep(0, 11) 
    temp = matrix(data = rep(c(marioHP,bowserHP,numMushroom,Action), 11), ncol = 4, byrow  = TRUE)
    SASRp_1 = cbind(temp, marioHP_Next[,1], rep(bowserHP_Next,11), rep(numMushroom,11), marioHP_Next[,2], Reward)
  }
  
  SASRp = rbind(SASRp_0, SASRp_1)
  colnames(SASRp) = c("xstart", "ystart", "zstart", "Action", "xprime", "yprime", "zprime", "prob", "reward")
  
  #If Mario's starting HP is 0, the game is over. 
  if(marioHP == 0){
    SASRp = "GAME OVER. TRY AGAIN?"
  }
  
  return(SASRp)
}

#so now just have to see which action is best. start in partition 1, work our way up. 
StateValueFunction = array(data = NA, dim = c(31,101,4), dimnames = list(M_HP_txt, B_HP_txt,n_msh_txt))
OptimalActionMat = array(data = NA, dim = c(31,101,4), dimnames = list(M_HP_txt, B_HP_txt,n_msh_txt))
StateValueFunction[,1,] = 0
StateValueFunction[1,,] = 0

#Calculating the state-action value for all the states in the state space. 
for (ms in 1:4){
  for (m in 2:31){
    for(b in 2:101){
      temp_mat = any_action(marioHP = M_HP[m], bowserHP = B_HP[b], numMushroom = n_msh[ms])  
      
      # Choosing action 0
      temp_mat_0 = temp_mat[temp_mat[,"Action"] %in% 0, ]
      # Choosing action 1
      temp_mat_1 = temp_mat[temp_mat[,"Action"] %in% 1, ]
      SVF_prime_0 = NULL
      SVF_prime_1 = NULL
      SVF_1 = NA
      
      # v(s') calculation
      for (i in 1:nrow(temp_mat_0)){
        SVF_prime_0[i] = StateValueFunction[temp_mat_0[i,"xprime"]+1, temp_mat_0[i,"yprime"] +1, temp_mat_0[i,"zprime"]+1]
      }
      temp_mat_0 = cbind(temp_mat_0, SVF_prime_0)
      SVF_0 = sum((temp_mat_0[,"SVF_prime_0"] + temp_mat_0[,"reward"])*temp_mat_0[,"prob"])
      
      if (length(temp_mat_1) != 0){
        for (j in 1:nrow(temp_mat_1)){
          SVF_prime_1[j] = StateValueFunction[temp_mat_1[j,"xprime"]+1, temp_mat_1[j,"yprime"] +1, temp_mat_1[j,"zprime"]+1]
        }
        temp_mat_1 = cbind(temp_mat_1, SVF_prime_1)
        SVF_1 = sum((temp_mat_1[,"SVF_prime_1"] + temp_mat_1[,"reward"])*temp_mat_1[,"prob"])
      }
      
      # Store optimal actions
      OptimalActionMat[m, b, ms] = which.max(c(SVF_0, SVF_1)) - 1 
      
      # Store state values
      StateValueFunction[m, b, ms] = max(SVF_0, SVF_1, na.rm = TRUE)
    }
  }
}
OptimalActionMat
round(StateValueFunction,3)

# (c) ------------------------------------------

#(i)
PolicyStateValueFunction[30+1,100+1,3+1]

#(ii)
StateValueFunction[30+1,100+1,3+1]

#(iii)
OptimalActionMat[12+1,22+1,1+1] 
#Attack is the best option. 

#(iv)

# Attacking isn't wise, eat mushroom instead 
OptimalActionMat[5+1,11+1,1+1]
SASRp = any_action(5,11,1)
SASRp = SASRp[SASRp[,"Action"] %in% 0,][,c("xprime","yprime","zprime", "prob")]
SVF = NULL
for (i in 1:6){
  SVF[i] = StateValueFunction[SASRp[i,1] +1, SASRp[i,2] +1, SASRp[i,3] +1]
}
sum( SVF * SASRp[,"prob"] )
#q(s={5,11,1},a=0) =  0.7966939

# Question 2 ------------------------------------------
# (a) ------------------------------------------
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

# (b) ------------------------------------------
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
# Policy 3 is optimal as it has the highest state-value

# Finding the optimal policy (double check)
optimal_s1 = Policies[which.max(VFAllPolicies[,1]),1]
optimal_s2 = Policies[which.max(VFAllPolicies[,2]),2]
c(optimal_s1, optimal_s2)

# (c) ------------------------------------------
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

# (d) ------------------------------------------
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

# (e) ------------------------------------------
for(i in 1:20){
  if(i==1){
    valueFunctionEstimates = PolicyEvaluation(c(0,0),c(2,2),SASRp,0.9,10)
  } else{
    valueFunctionEstimates = PolicyEvaluation(valueFunctionEstimates,optimalPolicyEstimates,SASRp,0.9,10)
  }
  optimalPolicyEstimates = PolicyImprovement(valueFunctionEstimates,SASRp,0.9)
}
valueFunctionEstimates
optimalPolicyEstimates

# (f) ------------------------------------------
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
ValueIteration(c(0,0),SASRp,0.9,2)
ValueIteration(c(0,0),SASRp,0.9,5)
ValueIteration(c(0,0),SASRp,0.9,25)
ValueIteration(c(0,0),SASRp,0.9,500)


# Question 3 ------------------------------------------
# (a) ------------------------------------------
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
      indexOfFirst11 = match(11,temp)
      temp[indexOfFirst11] = 1
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

# (b) ------------------------------------------
#Generates 500,000 Episodes of the BlackJack Game
Nsamplebystate = array(data=0,dim=c(10,10,2))
ValueFunctionEstim = array(data=0,dim=c(10,10,2))
for(i in 1:500000){
  currentEpisode = SimulateBlackJackEpisodeStick20(i)
  #jth index starting at 2 since first card player receives -> total is less than 12 guaranteed
  for(j in 2:length(currentEpisode[[2]])){
    #Only Capture States in which player total is between 12 and 21
    if(currentEpisode[[3]][j]<=21 && currentEpisode[[3]][j]>=12){
      currentDealersTotalMinus1= currentEpisode[[6]][1]-1
      currentPlayersTotalMinus11 = currentEpisode[[3]][j]-11
      currentUsableAcesPlus1 = currentEpisode[[4]][j]+1
      ValueFunctionEstim[currentDealersTotalMinus1,currentPlayersTotalMinus11,currentUsableAcesPlus1] = (1/(Nsamplebystate[currentDealersTotalMinus1,currentPlayersTotalMinus11,currentUsableAcesPlus1]+1))*(currentEpisode[[1]][1]-ValueFunctionEstim[currentDealersTotalMinus1,currentPlayersTotalMinus11,currentUsableAcesPlus1]) +ValueFunctionEstim[currentDealersTotalMinus1,currentPlayersTotalMinus11,currentUsableAcesPlus1]
      Nsamplebystate[currentDealersTotalMinus1,currentPlayersTotalMinus11,currentUsableAcesPlus1] = 1+ Nsamplebystate[currentDealersTotalMinus1,currentPlayersTotalMinus11,currentUsableAcesPlus1]
    }
  }
}

#Questions
#dealer's total is 7, the player's total is 14 and the player has no usable ace
ValueFunctionEstim[7-1,14-11,0+1]
#dealer's total is 4, the player's total is 16 and the player has one usable ace
ValueFunctionEstim[4-1,16-11,1+1]
#dealer's total is 4, the player's total is 21 and the player has no usable ace
ValueFunctionEstim[4-1,20-11,0+1]
# # of times among the 500,000 episodes such that: the dealer's total is 4, the player's total is 18 and the player has no usable ace
Nsamplebystate[4-1,18-11,0+1]

# (c) ------------------------------------------
gridPlayerTotal = 12:21
gridDealerTotal = 2:11
M <- mesh(gridPlayerTotal, gridDealerTotal)
surf3D(x = M$x, y = M$y, z = t(ValueFunctionEstim[,,1]), xlab="Player's Sum", ylab="Dealer Showing",
       zlab="Return",bty="g",border="black",ticktype = "detailed", colkey=FALSE, theta=-60 ,phi=30,
       main="No Usable Ace")
surf3D(x = M$x, y = M$y, z = t(ValueFunctionEstim[,,2]), xlab="Player's Sum", ylab="Dealer Showing",
       zlab="Return",bty="g",border="black",ticktype = "detailed", colkey=FALSE,theta=-60 ,phi=30,
       main="Usable Ace")
