# parallelisation of brute force

library(parallel)

no_of_cores <- detectCores()
no_of_cores


#' Brute Force
#'
#' @param x x \code{data.frame} containing objects with weights \code{w} and values \code{v} as columns.
#' @param W max weight of the knapsack
#' @param parallel Set TRUE if parallelize brute force search 
#' 
#' @return a list with max value and corresponding elements
#' @export
#'
#' @examples
#' data(knapsack_objects)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack_p <-
  function(x, W, parallel = FALSE){
    stopifnot("x is not a data.frame." = is.data.frame(x))
    stopifnot("data.frame must contain exactly two variables" = ncol(x)==2)
    stopifnot("data.frame must contain the two variables v and w" = all(colnames(x) %in% c("v","w")))
    stopifnot("v and w must be positive values" = all(x[,1:2]>0 ))
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
