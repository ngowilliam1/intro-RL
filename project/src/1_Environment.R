library(R6)

#Defining Maze Class
Maze <- R6Class("Maze",
                public = list(
                  maze = NA,
                  untouched_maze = NA,
                  nrows = NA,
                  ncols = NA,
                  status = "Not Over",
                  possible_actions = c(1,2,3,4),
                  x_rat_coordinate = NA,
                  y_rat_coordinate = NA,
                  current_rat_location = NA,
                  original_rat_location = NA,
                  current_return = 0,
                  current_reward_obtained = NA,
                  visitedStates = matrix(NA,ncol=2),
                  current_time_step = 1,
                  
                  initialize = function(maze =NA, original_rat_location=ratStartingPosition) {
                    #Constructor for Maze Class, should feed a matrix as a paramater to maze and optionally the rat's starting location.
                    self$original_rat_location <- original_rat_location
                    self$x_rat_coordinate <- original_rat_location[1]
                    self$y_rat_coordinate <- original_rat_location[2]
                    self$current_rat_location <- original_rat_location
                    self$visitedStates <- rbind(self$visitedStates,self$original_rat_location)
                    self$maze <- maze
                    self$untouched_maze <- maze
                    self$nrows <- nrow(maze)
                    self$ncols <- ncol(maze)
                    self$possibleActions()
                  },
                  
                  resetGame = function(){
                    #Method resets the game to the initial state.
                    self$x_rat_coordinate <- self$original_rat_location[1]
                    self$y_rat_coordinate <- self$original_rat_location[2]
                    self$current_rat_location <- self$original_rat_location
                    self$visitedStates <- matrix(self$original_rat_location,ncol=2)
                    self$maze <- self$untouched_maze
                    self$possibleActions()
                    self$current_return = 0
                    self$status = "Not Over"
                    self$current_reward_obtained = NA
                    self$current_time_step = 1
                  },
                  
                  
                  updateBoard = function(action){
                    #Method updates the board and the reward obtained by taking action a.
                    self$current_time_step = self$current_time_step + 1
                    self$possibleActionsUpdate()
                    #Case where action causes rat to hit the wall
                    if(!(action %in% self$possible_actions)){
                      self$current_reward_obtained = self$getReward("Wall")
                      self$current_return = self$current_return + self$current_reward_obtained
                    } else {
                      if(action==1){
                        self$x_rat_coordinate = self$x_rat_coordinate+1
                        self$updateCurrentRatLocation()
                      } else if(action==2){
                        self$y_rat_coordinate = self$y_rat_coordinate+1
                        self$updateCurrentRatLocation()
                      } else if(action==3){
                        self$x_rat_coordinate = self$x_rat_coordinate-1
                        self$updateCurrentRatLocation()
                      } else if(action==4){
                        self$y_rat_coordinate = self$y_rat_coordinate-1
                        self$updateCurrentRatLocation()
                      }
                      #Check if end game or going to a seen/unseen state
                      if(self$maze[self$y_rat_coordinate,self$x_rat_coordinate]==10){
                        self$current_reward_obtained = self$getReward("EndGame")
                        self$current_return = self$current_return + self$current_reward_obtained
                        self$status = "Game Over"
                      } else if(!(any(self$visitedStates[,1] %in% self$x_rat_coordinate & self$visitedStates[,2] %in% self$y_rat_coordinate))){
                        self$visitedStates = rbind(self$visitedStates,c(self$x_rat_coordinate,self$y_rat_coordinate))
                        self$current_reward_obtained = self$getReward("UnseenState")
                        self$current_return = self$current_return + self$current_reward_obtained
                      } else{
                        self$current_reward_obtained = self$getReward("SeenState")
                        self$current_return = self$current_return + self$getReward("SeenState")
                      }
                    }
                  },
                  
                  possibleActionsUpdate = function(){
                    #Method updates the possible_actions.
                    self$possible_actions = self$possibleActions()
                  },
                  
                  possibleActions = function(x=self$x_rat_coordinate,y=self$y_rat_coordinate){
                    #Method identifies which action cause the mouse to move and which actions are classified as "not possible actions" in which the mouse will not move.
                    possible_actions = NULL
                    if( y != 1 && self$maze[y-1,x] !=0){possible_actions = c(possible_actions,4)}
                    if( y != self$nrows && self$maze[y+1,x] !=0){possible_actions = c(possible_actions,2)}
                    if( x != 1 && self$maze[y,x-1] !=0){possible_actions = c(possible_actions,3)}
                    if( x != self$ncols && self$maze[y,x+1] !=0){possible_actions = c(possible_actions,1)}
                    return(possible_actions)
                  },
                  
                  getReward = function(subsequent){
                    #Method determines the reward obtained depending on the action selected in the updateBoard method.
                    if(subsequent=="EndGame"){
                      return (winningGameReward)
                    }else if (subsequent=="SeenState"){
                      return (seenStateReward)
                    }else if(subsequent=="UnseenState"){
                      return(unseenStateReward)
                    }else if(subsequent=="Wall"){
                      return(invalidMoveReward)
                    }
                  },
                  updateCurrentRatLocation =function(){
                    #Method returns the current (x,y) coordinate of the rat.
                    self$current_rat_location = c(self$x_rat_coordinate,self$y_rat_coordinate)
                  }
                )
)

