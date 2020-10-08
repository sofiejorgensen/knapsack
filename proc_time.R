#Questions
library(tidyverse)
objects <- function(n){
  data.frame(w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))
}

set.seed(42)
obj_16 <- knapsack_objects(16)
obj_500 <- knapsack_objects(500)
obj_1000000 <- knapsack_objects(1000000)

# Brute force
ptm <- proc.time()  
brute_force_knapsack(x = obj_16, W = 3500)
proc.time() - ptm


# Dynamic
ptm <- proc.time()  
knapsack_dynamic(x = obj_500, W = 3500)                 
proc.time() - ptm


# Greedy
ptm <- proc.time()  
greedy_knapsack(x = obj_1000000 , W = 3500)
proc.time() - ptm


