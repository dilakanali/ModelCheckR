test_that("normality_check returns a data frame", {

  fit <- lm(mpg ~ wt, data = mtcars)

  result <- normality_check(fit)

  expect_s3_class(result, "data.frame")

})
