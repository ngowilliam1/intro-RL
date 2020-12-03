#This code defines a function that draws the optimal action at each position in the maze. 
source("2_GenerateEpisode.R")

draw_grid = function(title){
  plot(x = rep(0,maze_size[1]), y = rep(NA,maze_size[2]), xlim = c(0,maze_size[2]), ylim = c(0,maze_size[2]), xlab= "", ylab = "", xaxs="i", yaxs="i", xaxt='n', yaxt='n')
  grid(nx = maze_size[1] , ny = maze_size[2], col = "black", lty = "solid", lwd = par("lwd"), equilogs = TRUE)
  title(main = title)
}

dir_arrow = function(bp,action){
  #Transforming Maze coordinates to cartesian coordinates
  bp[1] = bp[1] -1
  bp[2] = maze_size[2] - bp[2]
  #winning position
  if(all(bp == c(maze_size[1]-1,0))){return(rect(bp[1], bp[2], bp[1]+1, bp[2]+1, col = "gold"))}
  #draw arrows for valid positions
  if(action == 1){return(arrows(bp[1]+0.2, bp[2]+0.5, bp[1]+0.8, bp[2]+0.5, length = 0.1))} #right 
  if(action == 2){return(arrows(bp[1]+0.5, bp[2]+0.8, bp[1]+0.5, bp[2]+0.2, length = 0.1))} #down
  if(action == 3){return(arrows(bp[1]+0.8, bp[2]+0.5, bp[1]+0.2, bp[2]+0.5, length = 0.1))} #left
  if(action == 4){return(arrows(bp[1]+0.5, bp[2]+0.2, bp[1]+0.5, bp[2]+0.8, length = 0.1))} #up
  #draw red block for invalid positions   
  if(action == 5){return(rect(bp[1], bp[2], bp[1]+1, bp[2]+1, col = "red"))}
}

draw_board = function(title,XY_A){
  draw_grid(title)
  for(i in 1:nrow(invalid_positions)){
    dir_arrow(invalid_positions[i,1:2], invalid_positions[i,3])
  }
  for(i in 1:nrow(valid_positions)){
    dir_arrow(XY_A[i,1:2], XY_A[i,3])
  }
}

