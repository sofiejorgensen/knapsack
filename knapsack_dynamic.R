
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
  subset <- matrix(NA, nrow = n+1, ncol = W+1)
  value <- matrix(NA, nrow = n+1, ncol = W+1)
  
  for (j in 0:W+1) {
    value[1,j] <- 0
    subset[1,j] <- 0
  }
  
  for (i in 2:(n+1)){
    for (j in 0:W+1) {
      
      if (w[i-1] > j) {
        value[i,j] <- value[i-1,j]
        subset[i,j] <- 0
      } else {
        value[i,j] <- max(value[i-1, j], value[i-1, j-w[i-1]]+v[i-1])
        subset[i,j] <- 1
      }
    }
  }
  
  elements <- c()
  
  K <- W
  for(i in (n+1):2) {
    print(paste("i:",i))
    print(paste("K:",K))
    if(subset[i, K+1] == 1) {
      print(subset)
      elements <- append(elements,i-1)
      print(paste("elements:",elements))
      print(paste("w[i-1]:", w[i-1]))
      K <- K-w[i-1]
    } else print(paste("subset i=",i,"and K+1=",K+1, "equals 0"))
  }
  
  return(list("value" = value, "elements" = elements))
}


knapsack_objects <-
  data.frame(
    v = c(10,40,30,50),
    w = c(5,4,6,3)
  )
W <- 10

knapsack_dynamic(knapsack_objects, W)


# knapsack_objects <-
#   data.frame(
#     w = c(23, 26, 20, 18, 32, 27, 29, 26, 30, 27),
#     v = c(505, 352, 458, 220, 354, 414, 498, 545, 473, 543)
#   )
# W <- 67
# 
# knapsack_dynamic(knapsack_objects, W)
