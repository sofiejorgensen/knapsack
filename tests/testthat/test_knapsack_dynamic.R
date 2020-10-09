context("knapsack_dynamic")

suppressWarnings(RNGversion("3.5.9"))

data("knapsack_objects")


# data.frame with incorrect variable names
set.seed(42)
n <- 2000
knapsack_objects_2 <- data.frame(
  weight = sample(1:4000, size = n, replace = TRUE),
  value = runif(n = n, 0, 10000)
)

# Implement test suites
test_that("Function rejects errounous input.", {
  expect_error(knapsack_dynamic(x = knapsack_objects_2[1:8,], 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], "3500"))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = 3333.3))
  expect_error(knapsack_dynamic(x = list(v = c(10,40,30,50), w = c(5,4,6,3)), W = 10))
  expect_error(knapsack_dynamic(x = data.frame(v = c(-10,40,30,50), w = c(5,-4,6,3)), W = 10))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], -1))
})


test_that("Function return correct results.", {
  kd <- knapsack_dynamic(x = knapsack_objects[1:19,], W = 2445)
  expect_equal(object = round(kd$value), expected = 18965)
  expect_true(all(round(kd$elements) %in% c(5,7,13,15)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 1000)
  expect_equal(round(kd$value), 5362)
  expect_true(all(round(kd$elements) %in% 7))
  
  st <- system.time(kd <- knapsack_dynamic(x = knapsack_objects[1:20,], W = 0))
  expect_true(as.numeric(st)[2] >= 0.00)
})

test_that("Correct object is returned", {
  expect_silent(gk <- greedy_knapsack(x = knapsack_objects[1:20,], W = 2000))
  expect_named(gk, c("value", "elements"))
})