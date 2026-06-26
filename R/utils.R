#' Différence d'ordre n d'une série
#'
#' @param n ordre du décalage (défaut : 1)
#' @param x vecteur numérique
#' @return vecteur de même longueur que x, avec n NA en tête
#' @export
delta <- function(n = 1, x) {
  diff.x <- diff(x, lag = n)
  c(rep(NA, n), diff.x)
}


#' Désaisonnalisation linéaire d'une série
#'
#' Estime une tendance linéaire par MCO et retourne les résidus.
#'
#' @param serie vecteur numérique
#' @return résidus de la régression sur une tendance linéaire
#' @export
detrend <- function(serie) {
  data <- data.frame(temps = seq_along(serie), serie)
  estimation <- lm(serie ~ temps, data)
  residuals(estimation)
}
