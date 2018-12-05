# Advent of code 2018 day 1
# https://adventofcode.com/2018/day/1

## @knitr day1part1
input <- data.table::fread(here::here("input","day1.txt"),header = F)
x <- input$V1
sum(x)

## @knitr day1part2
l <- x
cs <- cumsum(l)[duplicated(cumsum(l))]
i <- 0
while(length(cs) == 0) {
  i <- i+1
  l <- c(l, x)
  cs <- cumsum(l)[duplicated(cumsum(l))]
}

cs[[1]]
