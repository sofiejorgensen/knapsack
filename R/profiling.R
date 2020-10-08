# devtools::install_github("hadley/lineprof")
library(lineprof)

f <- function(...) {
  pause(0.1)
  knapsack_dynamic(knapsack_objects[1:8,],2000)
}

g <- function(...) {
  pause(0.1)
  brute_force_knapsack(knapsack_objects[1:8,],2000)
}

h <- function(...) {
  pause(0.1)
  greedy_knapsack(knapsack_objects[1:8,],2000)
}