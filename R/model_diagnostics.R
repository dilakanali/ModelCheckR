#' Comprehensive Model Diagnostics
#'
#' Summarizes key regression assumptions including
#' normality, heteroscedasticity, multicollinearity,
#' and influential observations.
#'
#' @param model A fitted linear model object.
#'
#' @return A data frame summarizing diagnostic results.
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp + disp, data = mtcars)
#' model_diagnostics(fit)
#'
#' @export

model_diagnostics <- function(model){

  if (!inherits(model, "lm")) {
    stop("model must be an object of class 'lm'")
  }

  # Normality
  norm_result <- normality_check(model)

  normality_status <- ifelse(
    norm_result$P_Value > 0.05,
    "Pass",
    "Fail"
  )

  # Heteroscedasticity
  hetero_result <- hetero_check(model)

  hetero_status <- ifelse(
    hetero_result$P_Value > 0.05,
    "Pass",
    "Fail"
  )

  # Multicollinearity
  vif_result <- multicol_check(model)

  multicol_status <- ifelse(
    max(vif_result$VIF) < 5,
    "Pass",
    "Warning"
  )

  # Outliers
  outlier_result <- outlier_check(model)

  outlier_status <- ifelse(
    any(outlier_result$Flag == "Influential"),
    "Warning",
    "Pass"
  )

  result <- data.frame(
    Assumption = c(
      "Normality",
      "Homoscedasticity",
      "Multicollinearity",
      "Influential Observations"
    ),
    Status = c(
      normality_status,
      hetero_status,
      multicol_status,
      outlier_status
    ),
    row.names = NULL
  )

  return(result)

}
