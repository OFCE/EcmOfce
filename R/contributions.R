#' Contributions of each determinant to the dynamic simulation
#'
#' For each exogenous variable in the ECM, computes the marginal contribution to
#' the simulated quarterly growth rate (delta-log) of the endogenous variable,
#' relative to a baseline where all exogenous variables are frozen at their
#' values at \code{debut_graph}. Contributions are additive.
#'
#' The algorithm runs \code{\link{simulation_dynamique}} once per exogenous
#' variable: each variable is "unfrozen" one at a time (in the order they appear
#' in the formula), and the marginal contribution is the difference between
#' successive simulations.
#'
#' @param estim \code{lm} object from MCE estimation; the dependent variable
#'   must be of the form \code{delta(1, log(...))}
#' @param data data.frame with at least a \code{date} column and all equation
#'   variables in levels
#' @param debut_graph start date for the decomposition; if \code{NULL}, the
#'   first non-NA date in the model is used
#' @return data.frame with columns:
#'   \describe{
#'     \item{date}{date}
#'     \item{dlog_obs}{observed delta-log of the endogenous variable}
#'     \item{dlog_sim}{simulated delta-log (full dynamic simulation)}
#'     \item{diff.passe}{contribution of initial conditions (baseline)}
#'     \item{diff.VAR}{marginal contribution of each exogenous variable (one column per variable in the formula)}
#'     \item{diff.residu}{residual: \code{dlog_obs - dlog_sim}}
#'   }
#' @export
contribution_determinants <- function(estim, data, debut_graph = NULL) {
  vars       <- all.vars(formula(estim))
  endog_dlog <- names(estim$model)[1]

  # Dynamic simulation with true exogenous values
  data_dynam <- simulation_dynamique(
    estim = estim, data = data,
    debut_graph = debut_graph, debut_simul = debut_graph
  )
  date_debut <- dplyr::first(data_dynam$date)

  # Freeze all variables at their initial values
  data_exo    <- dplyr::select(data, date, dplyr::all_of(vars))
  values_init <- as.list(dplyr::filter(data_exo, date == date_debut))
  data_init   <- dplyr::mutate(
    data_exo,
    dplyr::across(
      .cols = dplyr::all_of(vars),
      .fns  = ~ dplyr::if_else(date > date_debut, values_init[[dplyr::cur_column()]], .x)
    )
  )

  # Baseline simulation (all exogenous frozen) = contribution of "passe"
  data_dynam_init <- simulation_dynamique(
    estim = estim, data = data_init,
    debut_graph = debut_graph, debut_simul = debut_graph
  )

  # Base table: observed and simulated delta-log
  data_det <- dplyr::select(
    data_dynam, date, observe, simul_dynamique,
    dlog_sim = !!rlang::sym(endog_dlog)
  )
  data_det <- dplyr::mutate(data_det, dlog_obs = delta(1, log(observe)))
  data_det <- dplyr::left_join(
    data_det,
    dplyr::select(data_dynam_init, date, simul.init = !!rlang::sym(endog_dlog)),
    by = "date"
  )

  # For each exogenous variable: unfreeze it and compute marginal contribution
  data_init_change <- data_init
  for (var in vars[-1]) {
    data_init_change <- dplyr::select(data_init_change, -dplyr::all_of(var))
    data_init_change <- dplyr::mutate(data_init_change, !!var := data[[var]])
    data_dynam_var <- simulation_dynamique(
      estim = estim, data = data_init_change,
      debut_graph = debut_graph, debut_simul = debut_graph
    )
    simul_cols <- names(data_det)[grepl("^simul\\.", names(data_det))]
    last_col   <- dplyr::last(simul_cols)
    new_col    <- paste0("simul.", var)
    data_det   <- dplyr::left_join(
      data_det,
      dplyr::rename(
        dplyr::select(data_dynam_var, date, !!rlang::sym(endog_dlog)),
        !!new_col := !!rlang::sym(endog_dlog)
      ),
      by = "date"
    )
    data_det <- dplyr::mutate(
      data_det,
      !!paste0("diff.", var) := .data[[new_col]] - .data[[last_col]]
    )
  }

  # Final output: drop level columns, add residual, drop first row (NA from delta)
  data_det <- dplyr::select(
    data_det, date, dlog_obs, dlog_sim,
    diff.passe = simul.init, dplyr::starts_with("diff.")
  )
  data_det <- dplyr::mutate(data_det, diff.residu = dlog_obs - dlog_sim)
  dplyr::slice(data_det, -1)
}
