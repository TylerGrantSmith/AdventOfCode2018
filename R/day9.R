play_game <- function(pc, mc) {
  circle = c(1, 0, 2)
  player = 5
  scores = rep(0, pc)
  
  for(i in 3:mc) {
    if (i %% 23 == 0) {
      circle <- c(circle[(i -5):i], circle[1:(i-6)])
      scores[player] = (scores[player] %||% 0) + i + circle[[i-1]]
      circle <- c(circle[1:(i-2)], circle[i])
    } else {
      circle <- c(circle[2:i], circle[1], i)
      player = (player + 1) %% pc
    }
  }
  return(scores)
}

