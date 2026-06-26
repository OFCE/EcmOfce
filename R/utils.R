#' Lagged difference of order n
#'
#' @param n lag order (default: 1)
#' @param x numeric vector
#' @return vector of same length as x, with n leading NAs
#' @export
delta <- function(n = 1, x) {
  diff.x <- diff(x, lag = n)
  c(rep(NA, n), diff.x)
}


#' Format numbers for display (French typography)
#'
#' Formats numbers with a narrow non-breaking space as thousands separator
#' and a comma as decimal mark, suitable for OFCE tables and tooltips.
#'
#' @param x numeric vector
#' @param digits number of decimal places (default: 1)
#' @return character vector of formatted values
#' @export
fmt_val <- function(x, digits = 1) {
  formatC(x = x, digits = digits, big.mark = ".", decimal.mark = ",", format = "f")
}


#' Linear detrending of a series
#'
#' Estimates a linear trend by OLS and returns the residuals.
#'
#' @param serie numeric vector
#' @return residuals from regression on a linear trend
#' @export
detrend <- function(serie) {
  data <- data.frame(temps = seq_along(serie), serie)
  estimation <- lm(serie ~ temps, data)
  residuals(estimation)
}
