context("Test all functions")

library(mirt)
library(strucchange)
library(scDIFtest)
library(testthat)


### data and model
dat <- expand.table(LSAT7)
nObs <- dim(dat)[1]
mod <- mirt(dat, 1, itemtype = "2PL", constr = list(c(2, 1)),
            verbose = FALSE)
mod2 <- mirt(dat, 1, itemtype = "Rasch", verbose = FALSE)

set.seed(5486)
logical <- sample(size= nObs, c(TRUE, FALSE), replace = TRUE)
metric <- rnorm(nObs)
ordered <- ordered(sample(1:5, size = nObs, replace = TRUE))
factor <- factor(sample(1:2, size = nObs, replace = TRUE))

t1 <- scDIFtest(mod, factor)
s1 <- summary(t1)
t2 <- scDIFtest(mod, decorrelate = FALSE)
s2 <- summary(t2)

### test get_colNrs function
test_that("get_colNrs works", {
  it_sel <- c(1:3)
  colNrs <- scDIFtest:::get_colNrs(mod, item_selection = it_sel)
  expect_is(colNrs, "list")
  expect_identical(length(colNrs), length(it_sel))
  expect_identical(colNrs[[2]], 2:3)
  expect_identical(colNrs[[1]], 1L)
})


### test scDIFtest gives expected output
test_that("scDIFtest gives expected output", {
  test <- scDIFtest(mod, logical)
  expect_is(test, "scDIFtest")
  expect_identical(test$info$test_info$DIF_covariate_name, "logical")
  expect_identical(test$info$test_info$stat_name, "Lagrange Multiplier Test for Unordered Groups")
  
  test <- scDIFtest(mod, metric)
  expect_identical(test$info$test_info$DIF_covariate_name, "metric")
  expect_identical(test$info$test_info$stat_name, "Double Maximum Test")
  
  test <- scDIFtest(mod, ordered)
  expect_identical(test$info$test_info$DIF_covariate_name, "ordered")
  expect_identical(test$info$test_info$stat_name, "Maximum Lagrange Multiplier Test for Ordered Groups")
  
  test <- scDIFtest(mod, factor)
  expect_identical(test$info$test_info$DIF_covariate_name, "factor")
  expect_identical(test$info$test_info$stat_name, "Lagrange Multiplier Test for Unordered Groups")
})


### test scDIFtest warnings and errors
test_that("scDIFtest returns warnings and errors", {
  expect_error(scDIFtest(mod, functional = "lmuo"), "functional")
})


### test scDIFtest-methods
test_that("summary method works", {
  expect_is(s1, "data.frame")
  sc1 <- sctest(mod, order.by = factor, functional = "LMuo", scores = mirt::estfun.AllModelClass, parm = 1)
  expect_equal(s1[1, 3], sc1$statistic[[1]])
  expect_equal(s1[1, 4], sc1$p.value[[1]])
  
  sc2 <- sctest(mod, scores = mirt::estfun.AllModelClass, 
                decorrelate = FALSE, parm = 1)
  expect_equal(s2[1, 3], sc2$statistic[[1]])
  expect_equal(s2[1, 4], sc2$p.value[[1]])
  expect_equal(s2[2, 4], NaN)
})


test_that("print method works", {
  expect_identical(print(t1), s1)
  expect_identical(print(t1, 2), NULL)
  
  expect_identical(print(t2, "Item.3"), NULL)
})


test_that("print method works", {
  expect_error(plot(t1))
  expect_equal(plot(t1, 3), NULL)
  expect_equal(plot(t2, 1), NULL)
  expect_warning(plot(t2, 3))
})
