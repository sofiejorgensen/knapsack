
# 0-1 knapsack, meaning there should only be 0 or 1 copies of each item x_i


  # Input:
  # Values (stored in array v)
  # Weights (stored in array w)
  # Number of distinct items (n)
  # Knapsack capacity (W)
  # NOTE: The array "v" and array "w" are assumed to store all relevant values
  # starting at index 1.

knapsack_dynamic <- function(x, W) {
  n <- nrow(x)
  w <- x$w
  v <- x$v
  elements <- c()
  
  value <- matrix(1, nrow = n, ncol = W)
  
  # value[1,] <- -1
  
  # Define function m so that it represents the maximum value we can get
  # under the condition: use first i items, total weight limit is j
  m <- function(i, j) {
    if (i == 0 || j <= 0) {
      value[i, j] <- 0
      return(value)
    }
    
    if (value[i - 1, j] == -1) {
      value[i - 1, j] <- m(i - 1, j)
    }
    
    if (w[i] > j) {
      value[i, j] <- m(i - 1, j)
    }
    else {
      if (value[i - 1, j - w[i]] == -1) {
        value[i - 1, j - w[i]] <- m(i - 1, j - w[i])
      }
      value[i, j] <- max(value[i - 1, j], value[i - 1, j - w[i]] + v[i])
      elements[i] <- i
      return(value)
    }
  }
  
  value <- m(n,W)
  return(list("value" = value, "elements" = elements))
}

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w = sample(1:4000, size = n, replace = TRUE),
    v = runif(n = n, 0, 10000)
  )
W <- 2000

knapsack_dynamic(x = knapsack_objects[1:8,], W = W)# recursive

   
   ## pseudo code below
    # if i == 0 or j <= 0 then:
    #   value[i, j] = 0
    # return
    # 
    # if (value[i-1,j] == -1) then:     # m[i-1, j] has not been calculated, we have to call function m
    #   value[i-1, j] = m(i-1,j)         
    # 
    # 
    # if w[i] > j then:                     # item cannot fit in the bag (THIS WAS MISSING FROM THE PREVIOUS ALGORITHM)
    #   value[i, j] = value[i-1, j]
    # 
    # else: 
    #   if (value[i-1, j-w[i]] == -1) then:     #m[i-1,j-w[i]] has not been calculated, we have to call function m
    #       value[i-1, j-w[i]] = m(i-1, j-w[i])
    # value[i, j] = max(value[i-1,j], value[i-1, j-w[i]] + v[i])
    # 




