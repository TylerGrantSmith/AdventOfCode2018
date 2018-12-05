# Advent of code 2018 day 4
# https://adventofcode.com/2018/day/4

## @knitr day4part1
library(lubridate)
input <- 
  fread(here::here("input","day4.txt"), header = F, sep = " ", fill = T) %>% 
  .[,.(DATE = map(V1, ymd),
       TIME = map(V2, hm),
       V3, V4, V5, V6)] %>% 
  .[, .(YEAR   = DATE %>% map_dbl(year),
        MONTH  = DATE %>% map_dbl(month),
        DAY    = DATE %>% map_dbl(day),
        HOUR   = TIME %>% map_dbl(hour),
        MINUTE = TIME %>% map_dbl(minute),
        V3, V4, V5, V6)]

input[V3 == "Guard", GUARD := V4 %>% str_remove("#") %>% as.numeric]
input[,ACTION:= V3]
setkeyv(input, c("YEAR","MONTH","DAY","HOUR","MINUTE", "GUARD"))

input[, GUARD := GUARD[1], .(cumsum(!is.na(GUARD)))] ## Neat fill
input[, NIGHT_OF := if_else(HOUR==23, shift(DAY,1, type = "lead"), DAY)]

input[, START := if_else(HOUR==23, 0, MINUTE), by = .(YEAR, MONTH, NIGHT_OF)]
input[, END := shift(MINUTE, 1, type = "lead", fill = 59), by = .(YEAR, MONTH, NIGHT_OF)]

input[, DURATION := END-START]

times <- data.table(TIME = 0:59)
sleepiest_guard <- input[,sum(DURATION),GUARD][order(-V1)][1, GUARD]
sleepiest_guard_dt <- input[ACTION == "falls"][GUARD==sleepiest_guard]

sleepiest_guard_sleepiest_minute <-
  sleepiest_guard_dt[times, 
                     on = .(START <= TIME), 
                     .(GUARD, TIME, START, END, DURATION,
                       cndn = (TIME >= START) & (TIME < END)),
                     allow.cartesian = T][cndn == T][,.N,TIME][order(-N)][1, TIME]



sleepiest_guard * sleepiest_guard_sleepiest_minute

## @knitr day4part2

sleep_rows <- input[ACTION == "falls"]
sleep_rows[times, 
           on = .(START <= TIME), 
           .(GUARD, TIME, START, END, DURATION,
             cndn = (TIME >= START) & (TIME < END)),
           allow.cartesian = T][cndn == T][,.N,.(TIME, GUARD)][order(-N)][1] %>% 
  .[,TIME * GUARD]


