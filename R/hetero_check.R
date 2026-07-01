#' Breusch-Pagan Test for Heteroscedasticity
#'
#' Performs the Breusch-Pagan test manually.
#'
#' @param model A fitted linear model object.
#'
#' @return A data frame containing the BP statistic,
#' degrees of freedom, p-value, and conclusion.
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp + disp, data = mtcars)
#' hetero_check(fit)
#'
#' @importFrom stats residuals lm model.matrix pchisq
#' @export

hetero_check <- function(model){

  # Input validation
  if (!inherits(model, "lm")) {
    stop("model must be an object of class 'lm'")
  }

  # Residuals
  e <- residuals(model)

  # Squared residuals
  e2 <- e^2

  # Auxiliary regression
  aux_model <- lm(
    e2 ~ model.matrix(model)[, -1, drop = FALSE]
  )

  # R-squared
  r2 <- summary(aux_model)$r.squared

  # Sample size
  n <- length(e)

  # Number of predictors
  k <- ncol(model.matrix(model)) - 1

  # BP statistic
  bp_stat <- n * r2

  # p-value
  p_value <- 1 - pchisq(bp_stat, df = k)

  # Conclusion
  conclusion <- ifelse(
    p_value > 0.05,
    "Homoscedastic",
    "Heteroscedastic"
  )

  result <- data.frame(
    BP_Statistic = round(bp_stat, 4),
    DF = k,
    P_Value = round(p_value, 4),
    Conclusion = conclusion
  )

  return(result)

}
