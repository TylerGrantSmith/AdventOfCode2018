# Advent of code 2018 day 3
# https://adventofcode.com/2018/day/3

## @knitr day3part1
# Part 1
input <- 
  fread(here::here("input","day3.txt"), header = F, sep = " ") %>% 
  set_names(c("id","@","location","dimensions"))

cleaned <-
  input %>% 
  select(-`@`) %>% 
  mutate(location = 
           sub("\\:","", location)) %>% 
  separate(location, c("left","top"), ",") %>% 
  separate(dimensions, c("width","height"), "x") %>%
  mutate_at(vars(-id),as.integer) %>% 
  setDT()

cleaned[,expand.grid(left:(left+width-1), top:(top+height-1)), by = id] %>% 
  .[,.N,by = .(Var1, Var2)] %>% 
  .[N > 1] %>% 
  nrow()

## @knitr day3part2
# Part 2
grid <- cleaned[,expand.grid(left:(left+width-1), top:(top+height-1)), by = id]
dupes <- grid[,.(id, .N),by = .(Var1, Var2)][N > 1]
setdiff(grid$id, dupes$id)
