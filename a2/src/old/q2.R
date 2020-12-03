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
#Finding the optimal policy
optimal_s1 = Policies[which.max(VFAllPolicies[,1]),1]
optimal_s2 = Policies[which.max(VFAllPolicies[,2]),2]
c(optimal_s1, optimal_s2)

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
#Applying value iteration with 2,5,25, and 500 iterations
niter_vec = c(2,5,25,500)
Answer = NULL
for(i in 1:4){
  Answer[[i]] = ValueIteration(StartVF = c(0,0), SASRp = SASRp, DF = 0.9, niter = niter_vec[i])
}
Answer
