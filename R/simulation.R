#' Simulation dynamique d'un modèle à correction d'erreur
#'
#' À partir d'une date de départ, remplace l'endogène observée par sa valeur
#' simulée à chaque période, puis prédit le delta log à la période suivante.
#' La variable dépendante doit être de la forme \code{delta(1, log(...))}.
#'
#' @param estim objet \code{lm} issu de l'estimation du MCE
#' @param data data.frame contenant au moins une colonne \code{date} et toutes
#'   les variables de l'équation
#' @param debut_graph première date affichée dans les données retournées ;
#'   si \code{NULL}, égale à \code{debut_simul}
#' @param debut_simul date à partir de laquelle on bascule en simulation ;
#'   si \code{NULL}, première date sans NA dans le modèle
#' @return data.frame avec les colonnes \code{date}, \code{simul_dynamique},
#'   \code{observe} et \code{residu}, filtré à partir de \code{debut_graph}
#' @export
simulation_dynamique <- function(estim, data, debut_graph = NULL, debut_simul = NULL) {
  vars <- all.vars(formula(estim))
  endog <- vars[1]
  endog_dlog <- names(estim$model)[1]

  if (!stringr::str_detect(endog_dlog, "^delta\\(1, log\\(")) {
    stop("La variable dependante n'est pas un delta(log(.)).")
  }

  var_lt <- grep("^(?!.*delta).*lag.*$", names(estim$coefficients),
    value = TRUE, perl = TRUE
  )
  var_force_rappel <- var_lt[grepl(endog, var_lt)]
  if (length(var_force_rappel) == 0) {
    stop("La variable dependante retardee n'est pas trouvee dans l'equation.")
  }

  data_dynam <- model.frame(
    formula(estim),
    data = tibble::column_to_rownames(data, var = "date"),
    na.action = na.pass
  )
  data_dynam <- tibble::rownames_to_column(data_dynam, var = "date")

  complete_rows <- complete.cases(data_dynam)
  first_complete_date <- data_dynam$date[which(complete_rows)[1]]

  if (is.null(debut_simul)) {
    debut_simul <- first_complete_date
  } else if (debut_simul < first_complete_date) {
    debut_graph <- first_complete_date
  }
  if (is.null(debut_graph)) debut_graph <- debut_simul

  data_dynam <- dplyr::select(data, date, dplyr::all_of(vars))
  data_dynam <- dplyr::mutate(data_dynam, !!names(estim$model)[1] := NA)

  start_index <- which(data_dynam$date == debut_simul)
  data_dynam[start_index:nrow(data_dynam), endog] <- NA

  for (i in start_index:nrow(data_dynam)) {
    data_dynam[[endog_dlog]][i] <- predict(estim, newdata = data_dynam)[i]
    data_dynam[[endog]][i] <- data_dynam[[endog]][i - 1] * exp(data_dynam[[endog_dlog]][i])
  }

  data_dynam <- dplyr::rename(data_dynam, simul_dynamique = !!rlang::sym(endog))
  data_dynam <- dplyr::left_join(
    data_dynam,
    dplyr::rename(dplyr::select(data, date, dplyr::all_of(endog)), observe = !!rlang::sym(endog)),
    by = "date"
  )
  data_dynam <- dplyr::mutate(data_dynam, residu = observe - simul_dynamique)
  dplyr::filter(data_dynam, date >= debut_graph)
}
