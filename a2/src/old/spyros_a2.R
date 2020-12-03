#STAT 497H ------- Reinforcement Learning ------ Assignment II -------

#install.packages("plot3D")
#library(plot3D)

####### QUESTION 1 #######
#1(a)
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
round(PolicyStateValueFunction[,,], 3)

#1(b)
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
      temp_mat_0 = temp_mat[temp_mat[,"Action"] %in% 0, ]
      temp_mat_1 = temp_mat[temp_mat[,"Action"] %in% 1, ]
      SVF_prime_0 = NULL
      SVF_prime_1 = NULL
      SVF_1 = NA
      
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
      
      OptimalActionMat[m, b, ms] = which.max(c(SVF_0, SVF_1)) - 1 
      StateValueFunction[m, b, ms] = max(SVF_0, SVF_1, na.rm = TRUE)
    }
  }
}
OptimalActionMat
round(StateValueFunction,3)

#1(c)
  #(i)
  PolicyStateValueFunction[30+1,100+1,3+1]
  
  #(ii)
  StateValueFunction[30+1,100+1,3+1]
  
  #(iii)
  OptimalActionMat[12+1,22+1,1+1] 
  #Attack is the best option. 
  
  #(iv)
  OptimalActionMat[5+1,11+1,1+1] #Indeed attack is suboptimal
  SASRp = any_action(5,11,1)
  SASRp = SASRp[SASRp[,"Action"] %in% 0,][,c("xprime","yprime","zprime", "prob")]
  SVF = NULL
  for (i in 1:6){
    SVF[i] = StateValueFunction[SASRp[i,1] +1, SASRp[i,2] +1, SASRp[i,3] +1]
  }
  sum( SVF * SASRp[,"prob"] )
  #q(s={5,11,1},a=0) =  0.7966939

  
####### QUESTION 2 #######
#2(a)
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
colnames(SASRp) = c("s", "a", "s'", "r","  p(s′,r|s,a)")
SASRp

#2(b)
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
VFAllPolicies

#Optimal policy
c(Policies[which.max(VFAllPolicies[,1]),1], Policies[which.max(VFAllPolicies[,2]),2])

#2(c)
PolicyEvaluation = function(StartVF, Policy, SASRp, DF, niter){
  VFEstimateCurrent = StartVF
  VFEstimateNext = NULL
  for (n in 1:niter){
    #Only interested in state-action pairs which follows policy pi
    for (i in 1:2){
      SASRp_temp = matrix(data = SASRp[SASRp[,"s"] %in% i & SASRp[,"a"] %in% Policy[i],], ncol = 5)
      VFEstimateNext[i] = sum( (SASRp_temp[,4] + DF*VFEstimateCurrent[SASRp_temp[,3]]) * SASRp_temp[,5] )
    }
    VFEstimateCurrent = VFEstimateNext
  }
  return(VFEstimateNext)
}
PolicyEvaluation(StartVF = c(3,2), Policy = c(1,1), SASRp = SASRp, DF = 0.9, niter = 1)

#2(d)
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
  PolicyEstimate = c(which.max(Q[1,]), which.max(Q[2,]))
  return(PolicyEstimate)
}
PolicyImprovement(VFEstim = c(3,2), SASRp = SASRp, DF = 0.9)

#2(e)
GPI = function(StartVF, Policy, SASRp, DF, ncycles){
  niter = 10
  #First cycle
  PE = PolicyEvaluation(StartVF, Policy, SASRp, DF, niter)
  PI = PolicyImprovement(VFEstim = PE, SASRp, DF)
  #Remaining cycles
  for (i in 2:ncycles){
    PE = PolicyEvaluation(PE, PI, SASRp, DF, niter)
    PI = PolicyImprovement(PE, SASRp, DF)
  }
  Result = list(PE, PI)
  return(Result)
}
GPI(StartVF = c(0,0), Policy = c(2,2), SASRp = SASRp, DF = 0.9, ncycles = 20)

#2(f)
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
ValueIteration(c(0,0),SASRp,0.9,2)
ValueIteration(c(0,0),SASRp,0.9,5)
ValueIteration(c(0,0),SASRp,0.9,25)
ValueIteration(c(0,0),SASRp,0.9,500)


####### QUESTION 3 #######
#3(a)
SimulateBlackJackEpisodeStick20 = function(theseed){
  set.seed(theseed)
  #Starting cards. Player has 2 facing up; dealer has 1 facing up. 
  pr_card = c(rep(1,8), 4, 1) / 13
  cards_p = sample(2:11, 2, prob = pr_card, replace = TRUE)
  cards_d = sample(2:11, 2, prob = pr_card, replace = TRUE)
  sum_p = cumsum(cards_p)
  sum_d = cumsum(cards_d)
  
  #Player's Actions: Hit if sum is less than 20, otherwise stay.
  if (sum_p[2] != 21){
    #In case player's starting cards are Ace x Ace
    Ace_usable = ifelse(any(cards_p == 11), sum(cards_p == 11), 0)
    if (Ace_usable > 1){
      while (Ace_usable > 1) {
        Ace_usable = Ace_usable - 1
        cards_p[min(which(cards_p ==11))] = 1 
      }
      sum_p = cumsum(cards_p)
    }
    while (max(sum_p) < 20){
      cards_p[length(cards_p) +1] = sample(2:11, 1, prob = pr_card, replace = TRUE) #Hit
      if ( all(cards_p != 11) ){
        Ace = 0    #Unusable
      } else if (any(cards_p == 11)){
        if (sum(cards_p) <= 21){
          Ace = 1  #Usable
        } else {
          cards_p[min(which(cards_p ==11))] = 1  #Ace now counts as 1. Use min(.) in case there are two+ aces. 
          Ace = length(which(cards_p ==11)) #Can still get usable (if 2 aces in hand), otherwise always 0.
        }
      }
      Ace_usable = c(Ace_usable, Ace)
      sum_p = c(sum_p, sum(cards_p))
      #current_sum[length(cards_p) -1] = sum(cards_p)
    }
  }
  
  #If the player busts, the episode ends with reward -1. Otherwise, it is the dealers turn to act. Reward TBD.
  if (sum(cards_p) > 21){
    reward = -1 
  } else {
    #In case multiple aces
    Ace_usable_d = ifelse(any(cards_d == 11), sum(cards_d == 11), 0)
    if (Ace_usable_d > 1){
      while (Ace_usable_d > 1) {
        Ace_usable_d = Ace_usable_d - 1
        cards_d[min(which(cards_d ==11))] = 1 
      }
      sum_d = c(sum_d, sum(cards_d))
    }
    #Hit if sum is less than 17, otherwise stay.
    while (sum(cards_d) < 17){
      cards_d[length(cards_d) +1] = sample(2:11, 1, prob = pr_card, replace = TRUE) #Hit
      if (any(cards_d == 11) && sum(cards_d) > 21){
        cards_d[which(cards_d ==11)] = 1
      }
      sum_d = c(sum_d, sum(cards_d))
    }
    if ( ((sum(cards_p) > sum(cards_d)) & sum(cards_d) < 22) || sum(cards_d) > 21 ){
      reward = 1 
    } else if ( sum(cards_p) == sum(cards_d) ){
      reward = 0
    } else if (sum(cards_p) < sum(cards_d) & sum(cards_d) < 22){
      reward = -1
    } 
  }
  #Changing aces back to 11's
  cards_p[which(cards_p == 1)] = 11
  cards_d[which(cards_d == 1)] = 11
  #Checking for a "natural".  
  if (sum_p[2] == 21){
    Ace_usable = 1
    cards_d = cards_d[1:2]
    sum_d = cumsum(cards_d)
    if (sum_d[2] == 21){
      reward = 0
    } else {
      reward = 1
    }
  }
  
  Result = list(reward, cards_p, sum_p, Ace_usable, cards_d, sum_d)
  return(Result)
}

# 3(b)
#Generating 500,000 Episodes of the BlackJack Game
Nsamplebystate = array(data=0,dim=c(10,10,2))
ValueFunctionEstim = array(data=0,dim=c(10,10,2))
for(i in 1:500000){
  currentEpisode = SimulateBlackJackEpisodeStick20(i)
  #jth index starting at 2 since first card player receives -> total is less than 12 guaranteed
  for(j in 2:length(currentEpisode[[2]])){
    #Only Capture States in which player total is between 12 and 21
    if(currentEpisode[[3]][j]<=21 && currentEpisode[[3]][j]>=12){
      xx= currentEpisode[[6]][1]-1
      yy = currentEpisode[[3]][j]-11
      zz = currentEpisode[[4]][j-1]+1
      ValueFunctionEstim[xx,yy,zz] = (1/(Nsamplebystate[xx,yy,zz]+1))*(currentEpisode[[1]][1]-ValueFunctionEstim[xx,yy,zz]) +ValueFunctionEstim[xx,yy,zz]
      Nsamplebystate[xx,yy,zz] = 1+ Nsamplebystate[xx,yy,zz]
    }
  }
}

#Questions
#dealer's total is 7, the player's total is 14 and the player has no usable ace
ValueFunctionEstim[7-1,14-11,0+1]
#dealer's total is 4, the player's total is 16 and the player has one usable ace
ValueFunctionEstim[4-1,16-11,1+1]
#dealer's total is 4, the player's total is 21 and the player has no usable ace
ValueFunctionEstim[4-1,21-11,0+1]
# # of times among the 500,000 episodes such that: the dealer's total is 4, the player's total is 18 and the player has no usable ace
Nsamplebystate[4-1,18-11,0+1]

# 3(c)
gridPlayerTotal = 12:21
gridDealerTotal = 2:11
M <- mesh(gridPlayerTotal, gridDealerTotal)
surf3D(x = M$x, y = M$y, z = t(ValueFunctionEstim[,,1]), xlab="Player's Sum", ylab="Dealer Showing",
       zlab="Return",bty="g",border="black",ticktype = "detailed", colkey=FALSE, theta=-60 ,phi=30,
       main="No Usable Ace")
surf3D(x = M$x, y = M$y, z = t(ValueFunctionEstim[,,2]), xlab="Player's Sum", ylab="Dealer Showing",
       zlab="Return",bty="g",border="black",ticktype = "detailed", colkey=FALSE,theta=-60 ,phi=30,
       main="Usable Ace")
