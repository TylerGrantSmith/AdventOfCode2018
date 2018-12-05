
## @knitr day5part1
library(stringr)
library(foreach)
input <- 
  readr::read_file(here::here("input","day5.txt"))

input <- input %>% str_remove("\n")

annihilators <- paste0(c(letters, LETTERS), c(LETTERS, letters), collapse = "|")

output <- input
while(str_detect(output, annihilators)) {
  output <- str_replace_all(output, annihilators, "")
}

nchar(output)

## @knitr day5part2
doParallel::registerDoParallel(10)

results <-
  foreach (letter = letters,
           .final = rbindlist,
           .packages = 'stringr')  %dopar% {
             remove = paste0(letter,"|",toupper(letter))
             output <- str_replace_all(input, remove, "")
             
             while(str_detect(output, annihilators)) {
               output <- str_replace_all(output, annihilators, "")
             }
             
             list(letter = letter, nchar = nchar(output))
           }
doParallel::stopImplicitCluster()

results[order(nchar)][1]