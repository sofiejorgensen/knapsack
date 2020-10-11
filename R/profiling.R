# # devtools::install_github("hadley/lineprof")
# library(lineprof)
# 
# f <- function(...) {
#   pause(0.1)
#   knapsack_dynamic(knapsack_objects[1:8,],2000)
# }
# 
# g <- function(...) {
#   pause(0.1)
#   brute_force_knapsack(knapsack_objects[1:8,],2000)
# }
# 
# h <- function(...) {
#   pause(0.1)
#   greedy_knapsack(knapsack_objects[1:8,],2000)
# }
# 
# Rprof(tmp <- tempfile(), line.profiling = TRUE)
# compareMethods()
# Rprof()
# summaryRprof()
# unlink(tmp)
# 
# lineprof(brute_force_knapsack(knapsack_objects[1:12,],2000), interval = 0.01, torture = FALSE)
# 
# # one operation takes a lot of time!
# lineprof(knapsack_dynamic(knapsack_objects[1:20,],3500), interval = 0.01, torture = FALSE)
# 
# lineprof(greedy_knapsack(knapsack_objects[1:500,],1000000), interval = 0.01, torture = FALSE)
# 
# tmp <- tempfile()
# Rprof(tmp, interval = 0.1)
# knapsack_dynamic_prof(knapsack_objects[1:18,],5000)
# Rprof(NULL)
# 
# library(profvis)
# 
# 
# profvis({knapsack_dynamic_prof(knapsack_objects[1:12,],200)}, interval = 0.01)
# 
# profvis({
#   knapsack_dynamic_prof <-
#   function(x, W) {
#     pause(0.1)
#     stopifnot("x is not a data.frame." = is.data.frame(x))
#     stopifnot("data.frame must contain exactly two variables" = ncol(x)==2)
#     stopifnot("data.frame must contain the two variables v and w" = colnames(x) == c("v","w")||colnames(x) == c("w", "v"))
#     stopifnot("v and w must be positive values" = x[,1:2] > 0)
#     pause(0.1)
#     n <- nrow(x)
#     w <- x$w
#     v <- x$v
#     
#     pause(0.1)
#     subset <- matrix(NA, nrow = n+1, ncol = W+1)
#     value <- matrix(NA, nrow = n+1, ncol = W+1)
#     pause(0.1)
#     
#     for (j in 0:W+1) {
#       pause(0.1)
#       value[1,j] <- 0
#       subset[1,j] <- 0
#     }
#     
#     pause(0.1)
#     
#     for (i in 2:(n + 1)) {
#       pause(0.1)
#       for (j in 0:W + 1) {
#         if (w[i - 1] > j) {
#           value[i, j] <- value[i - 1, j]
#           subset[i, j] <- 0
#         } else {
#           pause(0.1)
#           value[i, j] <- max(value[i - 1, j], value[i - 1, j - w[i - 1]] + v[i - 1])
#           if ((w[i - 1] <= j) &
#               all(value[i - 1, j - w[i - 1]] + v[i - 1] > value[i - 1, j])) {
#             pause(0.1)
#             subset[i, j] <- 1
#           } else {
#             pause(0.1)
#             subset[i, j] <- 0
#           }
#         }
#       }
#     }
#     
#     elements <- c()
#     K <- W
#     for(i in (n+1):2) {
#       
#       if(subset[i, K+1] == 1) {
#         pause(0.1)
#         elements <- append(elements,i-1)
#         
#         K <- K-w[i-1]
#       }
#     }
#     pause(0.1)
#     return(list("value" = round(value[nrow(value), ncol(value)]), "elements" = sort(elements)))
#   }
# }, interval = 0.001)
# 
# system.time(knapsack_dynamic(knapsack_objects[1:100,], 6000))
# system.time(brute_force_knapsack(knapsack_objects[1:18,], 5000))
# system.time(greedy_knapsack(knapsack_objects[1:10000,], 50000000))
# 
# Rprof()
# brute_force_knapsack(knapsack_objects[1:24,], 3500, parallel = TRUE)
# Rprof(NULL)
# summaryRprof()
# 
# Rprof()
# brute_force_knapsack(knapsack_objects[1:18,], 3500, parallel = FALSE)
# Rprof(NULL)
# summaryRprof()
# 
# 
# Rprof()
# knapsack_dynamic(knapsack_objects[1:100,], 6000)
# Rprof(NULL)
# summaryRprof()
# 
# Rprof()
# greedy_knapsack(knapsack_objects[1:10000,], 10000000)
# Rprof(NULL)
# summaryRprof()