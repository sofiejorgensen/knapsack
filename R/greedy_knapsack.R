#' Greedy Heuristic
#'
#' @param x 
#' @param W 
#'
#' @return a list with max value and corresponding elements
#' @export
#'
#' @examples
#' knapsack_objects <-
#' data.frame(
#'   v = c(10,40,30,50),
#'   w = c(5,4,6,3)
#' )
#' W <- 10
#' 
#' greedy_knapsack((knapsack_objects, W)
greedy_knapsack <-
function(x, W){
  stopifnot("x is not a data.frame." = is.data.frame(x))
  stopifnot("data.frame must contain exactly two variables" = ncol(x)==2)
  stopifnot("data.frame must contain the two variables v and w" = colnames(x) == c("v","w")||colnames(x) == c("w", "v"))
  stopifnot("v and w must be positive values" = x[,1:2]>0 )
  # Main
  frac <- x$v/x$w
  df <- data.frame(object = 1:length(frac), frac = frac, v = x$v, w = x$w)
  # Sort in decreasing order
  df_sorted <- df %>% 
    arrange(desc(frac))
  S1 <- df_sorted[cumsum(df_sorted$w) <= W,]
  S2 <- df_sorted[cumsum(df_sorted$w) > W,][1,]
  if(sum(S1$v) >= S2$v){
    value <- sum(S1$v)
    elements <- S1$object
  }else{
    if(S2$w <= W){
      value <- round(S2$v)
      elements <- S2$object
    }
  }
  return(list(value = round(value), elements = elements))
}
