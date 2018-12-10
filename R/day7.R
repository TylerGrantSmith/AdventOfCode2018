library(data.table)
library(tidyverse)

input <- fread(here::here("input","day7.txt"), header = F)

input <- input[,.(first= V2, then = V8)]
in_o <- input[order(first, then)]

steps <- LETTERS
no_limits <- setdiff(LETTERS, input$then)
available <- c()
block <- c()
active <- c("L")

while(length(active) <26) {
  available <- setdiff(input[first %in% active, then], active)
  unavailable <- input[then %in% available & !first %in% active, then] %>% unique()
  available <- c(no_limits, setdiff(available, unavailable))
  next_x <- sort(setdiff(available,block) %>% setdiff(active))[1]
  if(is.na(next_x)) next_x <- sort(steps %>% setdiff(active) %>% setdiff(in_o$then))[1]
  active <- c(active, next_x)
}
active %>% paste0(collapse="") %>% writeClipboard()

steps <- LETTERS
no_limits <- setdiff(LETTERS, input$then)
available <- c()
block <- c()
active <- c("L")

i = 0
wait = c(match(LETTERS, "L") + 60, 
         match(LETTERS, "N") + 60, 
         match(LETTERS, "R") + 60,
         match(LETTERS, "T") + 60,
         0)
load = c("L","N","R","T", NA)

finished = c()
while(i < 27) {
  finished <- c(load[wait == 1], finished)
  load[wait == 1] <- NA
  available <- setdiff(input[first %in% finished, then], finished)
  unavailable <- input[then %in% available & !first %in% active, then] %>% unique()
  available <- available %>% setdiff(unavailable) %>% setdiff(load)
  wait = pmax(wait - 1, 0)
  
  free_worker <- is.na(load)
  on_deck <- sort(available)[1:free_worker]
  wait[free_worker] <- 60 + match(on_deck, LETTERS)
  load[free_worker] <- on_deck
  i = i + 1
  cat(glue::glue("TIME: {i} {paste0(load,collapse='')} {paste0(wait, collapse = '')}"))
  cat("\n")
}
  
  next_x <- sort(setdiff(available,block) %>% setdiff(active))[1]
  if(is.na(next_x)) next_x <- sort(steps %>% setdiff(active) %>% setdiff(in_o$then))[1]
  active <- c(active, next_x)
  steps[Second := i, W1 :=]
}

