#' Normality Check for Model Residuals
#'
#' Performs a Shapiro-Wilk normality test on residuals from
#' a fitted linear regression model.
#'
#' @param model A fitted linear model object.
#'
#' @return A data frame containing the test statistic,
#' p-value, and conclusion.
#'
#' @examples
#' fit <- lm(mpg ~ wt, data = mtcars)
#' normality_check(fit)
#'
#' @importFrom stats residuals shapiro.test
#' @export

normality_check <- function(model){

  # Input validation
  if (!inherits(model, "lm")) {
    stop("model must be an object of class 'lm'")
  }

  # Residuals
  res <- residuals(model)

  # Shapiro-Wilk test
  test <- shapiro.test(res)

  # Decision
  conclusion <- if (test$p.value > 0.05) {
    "Normal residuals"
  } else {
    "Non-normal residuals"
  }

  # Output
  result <- data.frame(
    Statistic = round(unname(test$statistic), 4),
    P_Value = round(test$p.value, 4),
    Conclusion = conclusion
  )

  return(result)
}
