# parallelisation of brute force

library(parallel)

nCores <- detectCores()
cl <- makeCluster(nCores-1)


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
  function(x, W, parallel = FALSE) {
    stopifnot("x is not a data.frame." = is.data.frame(x))
    stopifnot("data.frame must contain exactly two variables" = ncol(x) ==
                2)
    stopifnot("data.frame must contain the two variables v and w" = all(colnames(x) %in% c("v", "w")))
    stopifnot("v and w must be positive values" = all(x[, 1:2] > 0))
    # Create all possible subsets
    subset_list <- c()
    value <- 0
    
    n <- nrow(x)
    
    for (i in 1:nrow(x)) {
      subset_list[[i]] <- combn(1:nrow(x), i, simplify = TRUE)
    }

    res <- data.frame(nrow = n, ncol = 2)
    
    # outer foor loop
    list_applier <-
      function(i) {
        k <- ncol(subset_list[[i]])
        
        res_list[i] <- mapply(FUN = j_applier, i = i, j = 1:k)
        # print(paste("i:", i))
        # print(paste("j:", j))
        return(value)
      }
    
    # inner for loop
    j_applier <- function(i, j) {
      res <- c()
      subset_tmp <- subset_list[[i]][, j]
      temp_w <- sum(x$w[subset_tmp])
      if (temp_w <= W) {
        temp_v <- sum(x$v[subset_tmp])
        if (temp_v > value) {
          res[i,j] <<- subset
          value[i,j] <<- temp_v
        }
      }
      return(res)
    }
    


    nCores <- detectCores()
    cl <- makeCluster(nCores-1)
    results <- clusterMap(cl = cl, fun = list_applier, i = 1:n)
    stopCluster(cl)

    print(result)
  
    # return(list(value = round(value), elements = res))
  }
    
