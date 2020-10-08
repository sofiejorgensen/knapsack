context("knapsack_dynamic")

suppressWarnings(RNGversion("3.5.9"))
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)
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
  expect_equal(round(kd$value), 1896544)
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
  expect_equal(round(kd$value), 16770)
  expect_true(all(round(kd$elements) %in% c(5, 8)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(kd$value), 16770)
  expect_true(all(round(kd$elements) %in% c(5, 8)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(kd$value), 15428)
  expect_true(all(round(kd$elements) %in% c(3, 8)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(kd$value), 15428)
  expect_true(all(round(kd$elements) %in% c(3, 8)))
  
  st <- system.time(kd <- knapsack_dynamic(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})