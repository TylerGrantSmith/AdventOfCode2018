# Advent of code 2018 day 2
# https://adventofcode.com/2018/day/2

## @knitr day2part1
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)

boxes <- fread(here::here("input","day2.txt"),header = F) %>% set_names("box")
box_count <- cbind(boxes, boxes[, purrr::map(letters, ~str_count(box, .)) %>% set_names(letters)])
box_count[,.(count2 = any(.SD == 2), count3 = any(.SD==3)), by = box, .SDcols = letters][,.(sum(count2),sum(count3))] %>% 
  reduce(`*`)

expand.grid(boxes[[1]],boxes[[1]]) %>% 
  as.tibble() %>% 
  mutate_all(funs(str_split(.,""))) %>% 
  mutate(diff = map2_int(Var1, Var2, ~sum(.x != .y))) %>% 
  filter(diff == 1) %>% 
  slice(1) %>% 
  select(-diff) %>% 
  unnest %>% 
  filter(Var1 == Var2) %>% 
  pull(Var1) %>% 
  reduce(paste0)