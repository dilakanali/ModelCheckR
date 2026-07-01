test_that("outlier_check returns a data frame", {

  fit <- lm(mpg ~ wt + hp + disp, data = mtcars)

  result <- outlier_check(fit)

  expect_s3_class(result, "data.frame")

})
