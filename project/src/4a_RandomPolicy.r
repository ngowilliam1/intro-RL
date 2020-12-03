source("1_Environment.R")

set.seed(0)
mazeObjectForRandomPolicy = Maze$new(maze=original_maze)


amountOfMovesAtEndOfEpisode = 0
returnAtEndOfEpisode = 0
gameStatusAtEndOfEpisode =""

#Testing performance indicators accross 10000 episodes
for(i in 1:10000){
  mazeObjectForRandomPolicy$resetGame()
  count = 0
  while(mazeObjectForRandomPolicy$status=="Not Over" &&  count<400){
    mazeObjectForRandomPolicy$updateBoard(sample(x=c(1,2,3,4),size=1))
    count = count + 1
  }
  amountOfMovesAtEndOfEpisode[i] = count
  returnAtEndOfEpisode[i] = mazeObjectForRandomPolicy$current_return
  gameStatusAtEndOfEpisode[i] = mazeObjectForRandomPolicy$status
}

#Number of episodes out of 10000 that ended
randomPolicyStatistics = list(length(returnAtEndOfEpisode[gameStatusAtEndOfEpisode=="Game Over"]),mean(returnAtEndOfEpisode[gameStatusAtEndOfEpisode=="Game Over"]))
names(randomPolicyStatistics) =c("Number of episodes out of 10000 that ended","Average Return out of the 10000 that ended")