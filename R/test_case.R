knapsack_objects <-
  data.frame(
    v = c(10,40,30,50),
    w = c(5,4,6,3)
  )
W <- 10

knapsack_dynamic(knapsack_objects, W)


knapsack_objects <-
  data.frame(
    w = c(23, 26, 20, 18, 32, 27, 29, 26, 30, 27),
    v = c(505, 352, 458, 220, 354, 414, 498, 545, 473, 543)
  )
W <- 67

