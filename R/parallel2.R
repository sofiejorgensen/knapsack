brute_force_knapsack_p <- function(x, W) {

  n <- nrow(x)
  
  subset_list <- c()
  for (i in 1:n){
    subset_list[[i]] <- combn(1:n, i, simplify = TRUE)
  }
  
  
  outer <- function(x, W, n, subset_list) {
    #result_df <- as.data.frame(matrix(NA, nrow = n, ncol = 2))

    res_list <- mapply(FUN = inner, i = 1:n, MoreArgs = list(x = x, W = W, subset_list = subset_list))
    
    return(list(res_list, subset_list[[3]][,48]))
    # colnames(result_df) <-  c("value", "elements")
    
    #return(result_df)
  }
  
  inner <- function(i, x, W, subset_list) {
    
    subset <- subset_list[[i]]
    j <- ncol(subset)

    val_calc <- function(x, W, subset, j) {
      #browser()
      temp_w <- sum(x[["w"]][subset[,j]])
      if (temp_w <= W) {
        return(sum(x[["v"]][subset[,j]]))
      } else {
        return(0)
      }
    }
    
    # nCores <- detectCores()
    # cl <- makeCluster(nCores-1)
    # clusterExport(cl = cl, varlist = c("subset", "x", "W"), envir=environment())
    # val_vector <- clusterMap(cl, fun = val_calc, 1:j, MoreArgs = list(subset, x, W))
    # stopCluster(cl)
    # return(val_calc)
    
    val_vector <- mapply(FUN = val_calc, j=1:j, MoreArgs = list(x, W, subset))
    
    #kombinera val_vector med elmenten från subset, välj ut max(val)
    #returnera lista med maxvärdet och tillhörande element
  }
  
  outer(x = x, W = W, n = n, subset_list = subset_list)

}

knapsack_obj <- function(n){
  data.frame(w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))
}
set.seed(42)
x <- knapsack_obj(n=2000)[1:8,]
brute_force_knapsack_p(x,3500)
brute_force_knapsack(x,3500)
