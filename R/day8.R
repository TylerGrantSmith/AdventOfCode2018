library(tidyverse)

input <- readr::read_file(here::here("input","day8.txt"))
input <- input %>% stringr::str_remove("\n") %>% stringr::str_split(" ") %>% unlist %>% as.integer()

# Part 1
read_node <- function(tree, total) {
  children = tree[[1]]
  m = tree[[2]]
  tree = tree[-(1:2)]
 
  score = list()
  value = 0
  
  if(children == 0) {
    score = sum(tree[1:m])
    total = total + score
    tree = tree[-(1:m)]
    return(list(tree, total, score))
  }
  
  for(i in 1:children) {
    l <- read_node(tree, total)
    tree <- l[[1]]
    total <- l[[2]]
    score[length(score)+1] <- l[[3]]
  }
  
  total = total + sum(tree[1:m])
  value = sum(unlist(score[tree[1:m]]))
  tree = tree[-(1:m)]
  return(list(tree, total, value))
}

read_node(input, 0)
