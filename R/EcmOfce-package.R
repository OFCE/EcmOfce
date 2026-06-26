#' @keywords internal
"_PACKAGE"

#' @importFrom rlang :=
#' @importFrom stats complete.cases fitted formula lag lm model.frame na.pass predict residuals terms
#' @importFrom dplyr first
NULL

# Supprime les notes R CMD CHECK sur les noms de colonnes utilisés dans aes()
utils::globalVariables(c(
  # colonnes ggplot dans plots.R
  "endog", "endog_niveau",
  "tooltip_resid", "tooltip_endog", "tooltip_pred",
  "observe", "simul_dynamique",
  "xmin", "xmax", "ymin", "ymax", "tooltip",
  "tooltip_obs", "tooltip_sim",
  # colonnes dplyr dans tableau.R
  "Groupe", "Variables", "dum"
))
