library(foreach)
library(data.table)
library(magrittr)
library(purrr)

# Part1
input <- fread(here::here("input","day6.txt"), header = F)

grid <- expand.grid(min(input$V1):max(input$V1), min(input$V2):max(input$V2)) %>% as.data.table()
grid_names <- grid %>% reduce(~paste0(.x,",",.y))
grid <- grid %>% as.matrix
rownames(grid) <- grid_names

locs <- as.matrix(input)
locs_names <-  paste0("Loc",1:nrow(locs))
rownames(locs) <- locs_names

chunk_size <- 10000
max_j <- ceiling(nrow(grid)/chunk_size)

results <- 
  foreach(j = 1:max_j,
          .combine = 'rbind',
          .final = as.data.table) %do% {
            chunk <- intersect(1:nrow(grid), 1:chunk_size + j * chunk_size)
            
            d <- dist(rbind(grid[chunk,],locs), "manhattan")
            m <- as.matrix(d)
            mm <- m[locs_names,grid_names[chunk]]
            
            min_dists <- apply(mm,2,min)
            
            foreach(point = names(min_dists),
                    .combine = 'rbind',
                    .final = as.data.table) %do% {
                      
                      point_dists <- mm[,point]
                      min_dist <- min(point_dists)
                      nearest_point <- names(point_dists)[!is.na(match(mm[,point], min_dist ))]
                      if(length(nearest_point) > 1) nearest_point <- "."
                      c(point, nearest_point)
                    }
          }
convex_hull <- rownames(locs)[chull(locs)]
results[!V2 %in% convex_hull,.N,V2][order(-N)][1]


# Part 2
grid <- expand.grid(-50:450, -50:450) %>% as.data.table()
grid_names <- grid %>% reduce(~paste0(.x,",",.y))
grid <- grid %>% as.matrix
rownames(grid) <- grid_names

locs <- as.matrix(input)
locs_names <-  paste0("Loc",1:nrow(locs))
rownames(locs) <- locs_names

chunk_size <- 10000
max_j <- ceiling(nrow(grid)/chunk_size)

results <- 
  foreach(j = 1:max_j) %do% {
            chunk <- intersect(1:nrow(grid), 1:chunk_size + j * chunk_size)
            d <- dist(rbind(grid[chunk,],locs), "manhattan")
            m <- as.matrix(d)
            mm <- m[locs_names,grid_names[chunk]]
            sum_dists <- apply(mm, 2, sum)
            sum_dists[sum_dists < 10000]
          }

reduce(results, c) %>% length





manhattan <- function(x1,y1,x2,y2) abs(x2-x1) + abs(y2-y1)
n = 0
counter = setDT(list(rep(0,nrow(locs))))
exclude = chull(locs)


for(x in 0:500) {
  for(y in 0:500) {
    s = 0
    best = Inf
    dbest = Inf
    for(i in 1:nrow(locs)) {
      d = manhattan(x,y,locs[i,1],locs[i,2])
      
      if (d < dbest) { best <- i; dbest <- d }
      else if (d == dbest) { best <- Inf }
      
      s = s + manhattan(x,y,locs[i,1],locs[i,2])
    }
    if(best != Inf) counter[best,V1 := V1 + 1]
    if(s < 10000) n = n + 1
  }
}
max(counter[])
n
