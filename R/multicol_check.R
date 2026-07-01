#' Multicollinearity Check Using VIF
#'
#' Computes Variance Inflation Factor (VIF)
#' for each predictor in a linear regression model.
#'
#' @param model A fitted linear model object.
#'
#' @return A data frame containing VIF values
#' and interpretation.
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp + disp, data = mtcars)
#' multicol_check(fit)
#'
#' @export

multicol_check <- function(model){

  # Input validation
  if (!inherits(model, "lm")) {
    stop("model must be an object of class 'lm'")
  }

  # Design matrix
  X <- model.matrix(model)

  # Remove intercept
  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, -1, drop = FALSE]
  }

  # Need at least two predictors
  if (ncol(X) < 2) {
    stop("VIF requires at least two predictors.")
  }

  predictor_names <- colnames(X)

  # Compute VIF manually
  vif_values <- sapply(seq_len(ncol(X)), function(i) {

    fit_i <- lm(
      X[, i] ~ X[, -i, drop = FALSE]
    )

    r2 <- summary(fit_i)$r.squared

    1 / (1 - r2)

  })

  interpretation <- ifelse(
    vif_values > 10,
    "High",
    ifelse(
      vif_values > 5,
      "Moderate",
      "Low"
    )
  )

  result <- data.frame(
    Variable = predictor_names,
    VIF = round(as.numeric(vif_values), 4),
    Interpretation = interpretation,
    row.names = NULL
  )

  return(result)

}
