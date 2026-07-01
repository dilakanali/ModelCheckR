test_that("model_diagnostics returns a data frame", {

  fit <- lm(mpg ~ wt + hp + disp, data = mtcars)

  result <- model_diagnostics(fit)

  expect_s3_class(result, "data.frame")

})
