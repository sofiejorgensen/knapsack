brute_force_knapsack_p <- function(x, W) {

  n <- nrow(x)
  
  subset_list <- c()
  for (i in 1:n){
    subset_list[[i]] <- combn(1:n, i, simplify = TRUE)
  }
  
  
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
    
    no_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(no_cores, setup_strategy = "sequential")
    doParallel::registerDoParallel(cl)
    
    parallel::clusterExport(cl = cl, varlist = c("subset", "x", "W"), envir=environment())
    val_vector <- parallel::clusterMap(cl, fun = val_calc, j=1:j, MoreArgs = list(x, W, subset))
    parallel::stopCluster(cl)
    
    value <- max(unlist(val_vector))
    elements <- which(max(unlist(val_vector)) ==  val_vector)
    return(list(value,elements))
  }
  
  outer(x = x, W = W, n = n, subset_list = subset_list)

}

knapsack_obj <- function(n){
  data.frame(w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))
}

set.seed(42)
x <- knapsack_obj(n=2000)[1:8,]
df <- brute_force_knapsack_p(x,3500)
df



#brute_force_knapsack(x,3500)
