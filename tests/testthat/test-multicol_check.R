test_that("multicol_check returns a data frame", {

  fit <- lm(mpg ~ wt + hp + disp, data = mtcars)

  result <- multicol_check(fit)

  expect_s3_class(result, "data.frame")

})
