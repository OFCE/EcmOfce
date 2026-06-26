#' @keywords internal
"_PACKAGE"

#' @importFrom rlang := sym .data
#' @importFrom stats complete.cases fitted formula lag lm model.frame na.omit na.pass predict residuals terms setNames update
#' @importFrom dplyr first
#' @importFrom lubridate %m+%
NULL

utils::globalVariables(c(
  # colonnes ggplot dans plots.R — simulation statique
  "endog", "endog_niveau",
  "tooltip_resid", "tooltip_endog", "tooltip_pred",
  # colonnes ggplot dans plots.R — simulation dynamique enrichie
  "observe", "simul_dynamique",
  "observe_g_trim", "simul_g_trim", "residu_g_trim",
  "observe_g_an", "simul_g_an", "residu_g_an",
  "residu", "var",
  # colonnes ggplot — comparaison simulations
  "courbe",
  "tooltip_obs", "tooltip_sim", "tooltip_sim_gt", "tooltip_obs_gt",
  "tooltip_resid_gt",
  # colonnes ggplot — rectangles indicatrices
  "xmin", "xmax", "ymin", "ymax", "tooltip",
  # colonnes dplyr dans tableau.R
  "Groupe", "Variables", "dum",
  # colonnes ggplot dans recursif.R (pivot_longer)
  "name", "value", "tstat",
  # colonnes dplyr dans contributions.R
  "dlog_obs", "dlog_sim", "simul.init"
))
