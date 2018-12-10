library(tidyverse)
library(ggplot2)
library(data.table)

input <- readr::read_file(here::here("input","day10.txt"))
input <- input %>% stringr::str_split("\n") %>% unlist()
input <- input[1:362] %>% stringr::str_extract_all(., pattern = "\\(?-?[0-9.]+\\)?",simplify = T)

clean <- input %>% as.tibble() %>%  mutate_all(as.numeric) %>% as.data.table()
test <- copy(clean)

bounds <- rep(NA_integer_,20000)

for (i in 1:20000) {
  test <- test[, c('V1','V2') := list(V1 + V3, V2 + V4)]
  bounds[i] = max(test$V1)-min(test$V1)  
}

clean[, .(V1 + time*V3, V2 + time*V4)] %>% 
  ggplot() + 
  aes(x = V1, y = V2) + 
  geom_point(size = 3, shape = 0) + 
  scale_y_reverse()

