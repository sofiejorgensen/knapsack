#' Brute Force
#'
#' @param x x \code{data.frame} containing objects with weights \code{w} and values \code{v} as columns.
#' @param W max weight of the knapsack
#' @param parallel boolean indicating whether or not parallel computing should be used
#'
#' @return a list with max value and corresponding elements
#' @export
#'
#' @examples
#' data(knapsack_objects)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack <-
function(x, W, parallel = FALSE){
  stopifnot("x is not a data.frame." = is.data.frame(x))
  stopifnot("data.frame must contain exactly two variables" = ncol(x)==2)
  stopifnot("data.frame must contain the two variables v and w" = all(colnames(x) %in% c("v","w")))
  stopifnot("v and w must be positive values" = all(x[,1:2]>0 ))
  stopifnot("W must be a positive value" = W >= 0)
  stopifnot("parallel must be a boolean (TRUE/FALSE)" = is.logical(parallel))
  # Create all possible subsets
  value <- 0
  n <- nrow(x)
  
  subset_list <- c()
  for (i in 1:n){
    subset_list[[i]] <- combn(1:n, i, simplify = TRUE)
  }
  
  if (parallel == FALSE) {
    outer <- function(x, W, n, subset_list) {
      res_list <- mapply(FUN = inner, i = 1:n, MoreArgs = list(x = x, W = W, subset_list = subset_list))
      # Number of elements in subset that gives max value
      i <- unlist(which.max(res_list[1,]))
      # Get index to identify the elements in subset_list
      j <- unlist(res_list[2,i])
      # Max value
      max_val <- round(max(unlist(res_list[1,])))
      return(list(value = max_val, elements = subset_list[[i]][,j]))
    }
    
    inner <- function(i, x, W, subset_list) {
      
      subset <- subset_list[[i]]
      j <- ncol(subset)
      
      val_calc <- function(x, W, subset, j) {
        temp_w <- sum(x[["w"]][subset[,j]])
        if (temp_w <= W) {
          return(sum(x[["v"]][subset[,j]]))
        } else {
          return(0)
        }
      }
      
      val_vector <- mapply(FUN = val_calc, j=1:j, MoreArgs = list(x, W, subset))
      
      value <- max(unlist(val_vector))
      elements <- which(max(unlist(val_vector)) ==  val_vector)
      return(list(value,elements))
    }
    
    outer(x = x, W = W, n = n, subset_list = subset_list)
  } else if (parallel == TRUE) {
    
    outer <- function(x, W, n, subset_list) {
      res_list <- mapply(FUN = inner, i = 1:n, MoreArgs = list(x = x, W = W, subset_list = subset_list))
      # Number of elements in subset that gives max value
      i <- unlist(which.max(res_list[1,]))
      # Get index to identify the elements in subset_list
      j <- unlist(res_list[2,i])
      # Max value
      max_val <- round(max(unlist(res_list[1,])))
      return(list(value = max_val, elements = subset_list[[i]][,j]))
    }
    
    inner <- function(i, x, W, subset_list) {
      
      subset <- subset_list[[i]]
      j <- ncol(subset)
      
      val_calc <- function(x, W, subset, j) {
        temp_w <- sum(x[["w"]][subset[,j]])
        if (temp_w <= W) {
          return(sum(x[["v"]][subset[,j]]))
        } else {
          return(0)
        }
      }

      parallel::clusterExport(cl = cl, varlist = c("subset", "x", "W"), envir=environment())
      val_vector <- parallel::clusterMap(cl, fun = val_calc, j=1:j, MoreArgs = list(x, W, subset))
      
      value <- max(unlist(val_vector))
      elements <- which(max(unlist(val_vector)) ==  val_vector)
      return(list(value,elements))
    }
    
    no_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(no_cores, setup_strategy = "sequential")
    # doParallel::registerDoParallel(cl)
    
    outer(x = x, W = W, n = n, subset_list = subset_list)
    parallel::stopCluster(cl)
    
  }
}
