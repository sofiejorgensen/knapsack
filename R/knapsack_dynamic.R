#' Dynamic Programming
#'
#' @param x x \code{data.frame} containing objects with weights \code{w} and values \code{v} as columns.
#' @param W max weight (discrete) of the knapsack 
#'
#' @return a list with max value and corresponding elements
#' @export
#'
#' @examples
#' data(knapsack_objects)
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_programming_in-advance_algorithm}
knapsack_dynamic <-
function(x, W) {
  stopifnot("x is not a data.frame." = is.data.frame(x))
  stopifnot("data.frame must contain exactly two variables" = ncol(x)==2)
  #stopifnot("data.frame must contain the two variables v and w" = colnames(x) == c("v","w")||colnames(x) == c("w", "v"))
  stopifnot("v and w must be positive values" = all(x[,1:2]>0 ))
  stopifnot("W must be a discrete value" = W%%1 == 0)
  stopifnot("W must be a positive value" = W >= 0)
  n <- nrow(x)
  w <- x$w
  v <- x$v
  subset <- matrix(NA, nrow = n+1, ncol = W+1)
  value <- matrix(NA, nrow = n+1, ncol = W+1)
  
  for (j in 0:W+1) {
    value[1,j] <- 0
    subset[1,j] <- 0
  }
  
  for (i in 2:(n + 1)) {
    for (j in 0:W + 1) {
      if (w[i - 1] > j) {
        value[i, j] <- value[i - 1, j]
        subset[i, j] <- 0
      } else {
        value[i, j] <- max(value[i - 1, j], value[i - 1, j - w[i - 1]] + v[i - 1])
        if ((w[i - 1] <= j) &
            all(value[i - 1, j - w[i - 1]] + v[i - 1] > value[i - 1, j])) {
          subset[i, j] <- 1
        } else {
          subset[i, j] <- 0
        }
      }
    }
  }
  
  elements <- c()
  
  K <- W
  for(i in (n+1):2) {
    
    if(subset[i, K+1] == 1) {

      elements <- append(elements,i-1)

      K <- K-w[i-1]
    }
  }
  
  return(list("value" = round(value[nrow(value), ncol(value)]), "elements" = sort(elements)))
}

