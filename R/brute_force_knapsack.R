#' Brute Force
#'
#' @param x 
#' @param W 
#'
#' @return a list with max value and corresponding elements
#' @export
#'
#' @examples
#' data(knapsack_objects)
#' brute_force_knapsack((x = knapsack_objects, W = 3500)
brute_force_knapsack <-
function(x, W){
  stopifnot("x is not a data.frame." = is.data.frame(x))
  stopifnot("data.frame must contain exactly two variables" = ncol(x)==2)
  stopifnot("data.frame must contain the two variables v and w" = colnames(x) == c("v","w")||colnames(x) == c("w", "v"))
  stopifnot("v and w must be positive values" = x[,1:2]>0 )
  # Create all possible subsets
  subset_list <- c()
  value <- 0
  for (i in 1:nrow(x)){
    subset_list[[i]] <- combn(1:nrow(x), i, simplify = TRUE)
  }
  # For each object
  for(i in 1:nrow(x)){
    # For each subset
    for(j in 1:ncol(subset_list[[i]])){
      subset <- subset_list[[i]][,j]
      temp_w <- sum(x$w[subset])
      # Compare computed weight with knapsack size W
      if(temp_w <= W){
        temp_v <- sum(x$v[subset])
        # Compare values
        if(temp_v > value){
          res <- subset
          value <- temp_v 
        }
      }
    }
  }
  return(list(value = round(value), elements = res))
}
