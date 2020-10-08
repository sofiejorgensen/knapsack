#' Dynamic Programming
#'
#' @param x 
#' @param W 
#'
#' @return a list with max value and corresponding elements
#' @export
#'
#' @examples
#' data(knapsack_objects)
#' knapsack_dynamic((x = knapsack_objects[1:8,], W = 3500)
knapsack_dynamic <-
function(x, W) {
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
