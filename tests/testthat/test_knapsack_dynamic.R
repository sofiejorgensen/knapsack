context("knapsack_dynamic")

suppressWarnings(RNGversion("3.5.9"))

data("knapsack_objects")
# set.seed(42)
# n <- 2000
# knapsack_objects <- data.frame(
#   w=sample(1:4000, size = n, replace = TRUE),
#   v=runif(n = n, 0, 10000)
# )
# 
# print(knapsack_objects[1:8,])

# Implement test suites
set.seed(42)
n <- 2000
knapsack_objects_2 <- data.frame(
  weight = sample(1:4000, size = n, replace = TRUE),
  value = runif(n = n, 0, 10000)
)

test_that("functions rejects errounous input.", {
  expect_error(knapsack_dynamic(x = knapsack_objects_2[1:8,], 3500))
  expect_error(knapsack_dynamic(x = list(v = c(10,40,30,50), w = c(5,4,6,3)), W = 10))
  expect_error(knapsack_dynamic(x = list(v = c(-10,40,30,50), w = c(5,-4,6,3)), W = 10))
})


test_that("Function return correct results.", {
  kd <- knapsack_dynamic(x = knapsack_objects[1:19,], W = 2445)
  expect_equal(object = round(kd$value), expected = 18965)
  expect_true(all(round(kd$elements) %in% c(5,7,13,15)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 1000)
  expect_equal(round(kd$value), 5362)
  expect_true(all(round(kd$elements) %in% c(7)))
  
  st <- system.time(kd <- knapsack_dynamic(x = knapsack_objects[1:20,], W = 3500))
  expect_true(as.numeric(st)[2] >= 0.00)
})

# Same tests as for brute force
test_that("Function return correct results.", {
  kd <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(kd$value), 17277)
  expect_true(all(round(kd$elements) %in% c(4, 5, 7)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(kd$value), 19912)
  expect_true(all(round(kd$elements) %in% c(5, 7, 10)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(kd$value), 11498)
  expect_true(all(round(kd$elements) %in% c(5, 7)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(kd$value), 13775)
  expect_true(all(round(kd$elements) %in% c(7, 10)))
  
  st <- system.time(kd <- knapsack_dynamic(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})