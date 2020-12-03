#In this file, we define our statespace, initialize some of our estimates, and define two important functions:
#(1) Function that calculates the epsilon-greedy policy based on estimate of Q
#(2) Function that generates episodes that follow a given policy

source("1_Environment.R")

#Describing the statespace 
maze_size = c(ncol(original_maze), nrow(original_maze))

valid_positions = which(original_maze != 0, arr.ind = TRUE) 
valid_positions = valid_positions[order(valid_positions[,1]),]
valid_positions = valid_positions[,c(2,1)] #(x,y)

invalid_positions = which(original_maze == 0, arr.ind = TRUE)
invalid_positions = invalid_positions[order(invalid_positions[,1]),]
invalid_positions = cbind(invalid_positions[,c(2,1)],5) #(x,y)

#map from (x,y) to (s)
nb_states = nrow(valid_positions)
states = 1:nb_states
get_state = function(maze){
  states[which(valid_positions[,1] %in% maze$current_rat_location[1] & valid_positions[,2] %in% maze$current_rat_location[2])]
}


EGPolicy = function(epsilon, Q){
  #Function that calculates the epsilon-greedy policy based on the current estimate of Q
  ImprovedEGPolicy = matrix(data = epsilon/4, nrow = nb_states, ncol =4)
  GreedyPolicy = max.col(Q)
  for(i in 1:nb_states){
    ImprovedEGPolicy[i, GreedyPolicy[i]] = 1 - epsilon + ImprovedEGPolicy[i, GreedyPolicy[i]]
  }
  return(ImprovedEGPolicy)
}


EpisodePolicy = function(theseed, Policy){
  #Generate an episode by following policy pi
  set.seed(theseed)
  SAR = NULL
  count = 0
  EpisodeMazeExit = Maze$new(maze=original_maze)
  while(EpisodeMazeExit$status == "Not Over"){
    count = count + 1 
    if(count > 400){break}
    if(EpisodeMazeExit$current_return == 0){
      aa = sample(1:4, 1, prob = Policy[1,])
      EpisodeMazeExit$updateBoard(aa)
      SAR[1:3] = c(1,aa, EpisodeMazeExit$current_reward_obtained)
    } else {
      ss = get_state(EpisodeMazeExit)
      aa = sample(1:4, 1, prob = Policy[ss,])
      EpisodeMazeExit$updateBoard(aa)
      SAR = cbind(SAR, c(ss, aa, EpisodeMazeExit$current_reward_obtained))
    }
  }
  rownames(SAR) = c("s", "a", "r")
  return(cbind(SAR,c(nb_states,1,NA))) #action 1 once in final state is meaningless, but required for SARSA algorithm
}

getmode = function(v) {
  #We will use this to determine most common greedy action a given state across 100 runs. 
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
