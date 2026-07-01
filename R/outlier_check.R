#' Outlier and Influence Check Using Cook's Distance
#'
#' Identifies influential observations in a linear
#' regression model using Cook's Distance.
#'
#' @param model A fitted linear model object.
#'
#' @return A data frame containing observation number,
#' Cook's Distance, and influence flag.
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' outlier_check(fit)
#'
#' @export

outlier_check <- function(model){

  # Input validation
  if (!inherits(model, "lm")) {
    stop("model must be an object of class 'lm'")
  }

  cook <- cooks.distance(model)

  n <- length(cook)

  threshold <- 4 / n

  flag <- ifelse(
    cook > threshold,
    "Influential",
    "Not Influential"
  )

  result <- data.frame(
    Observation = seq_along(cook),
    Cooks_Distance = round(as.numeric(cook), 4),
    Flag = flag,
    row.names = NULL
  )

  return(result)

}
