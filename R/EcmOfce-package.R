#' @keywords internal
"_PACKAGE"

#' @importFrom rlang := sym .data
#' @importFrom stats complete.cases fitted formula lm model.frame na.omit na.pass nobs predict residuals terms setNames update
#' @importFrom dplyr across arrange bind_rows case_when filter first full_join group_by left_join mutate rename select starts_with ungroup
#' @importFrom lubridate %m+%
#' @importFrom ggplot2 aes annotate element_text geom_bar geom_line geom_point ggplot ggtitle guide_legend guides labs scale_alpha_manual scale_color_manual scale_y_continuous theme
#' @importFrom ofce date_trim fmt_val scale_ofce_date theme_ofce
NULL

utils::globalVariables(c(
  # colonnes ggplot dans plots.R — simulation statique
  "endog", "endog_niveau",
  "tooltip_resid", "tooltip_endog", "tooltip_pred", "tooltip_endog_niv", "tooltip_pred_niv",
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
  "name", "value", "tstat", "Force de rappel",
  # colonnes dplyr dans contributions.R
  "dlog_obs", "dlog_sim", "simul.init"
))
