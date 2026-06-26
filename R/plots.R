#' Graphiques résidus et simulation statique (version simple)
#'
#' Représente la série observée vs la série simulée statiquement, et les résidus,
#' en utilisant les dates issues de \code{bank_emeraude} (usage interne OFCE).
#'
#' @param estim objet \code{lm} issu de l'estimation
#' @param data data.frame contenant au moins une colonne \code{date}
#' @return liste avec \code{plot_resid} et \code{plot_fit}
#' @export
make_plot_estim <- function(estim, data) {
  sample_length <- as.numeric(length(estim[["model"]][[1]]))
  length_bank <- as.numeric(length(dplyr::filter(data, date < date_fin)[["date"]]))
  init <- length_bank - sample_length + 1
  temps_estim <- data[["date"]]

  endog.value <- estim[["model"]][[1]]
  residus <- residuals(estim)
  prediction <- fitted(estim)
  graphic.data <- cbind(
    date = temps_estim,
    data.frame(endog = endog.value, residus = residus, prediction = prediction)
  )

  plot_fit <- ggplot2::ggplot(graphic.data, ggplot2::aes(x = date, y = endog)) +
    ggplot2::geom_point(color = "blue", size = 2) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = prediction)) +
    ggplot2::ggtitle("Observations / simulation statique") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  plot_resid <- ggplot2::ggplot(graphic.data, ggplot2::aes(x = date, y = residus)) +
    ggplot2::geom_bar(stat = "identity", color = "blue") +
    ggplot2::ggtitle("Residus de l'estimation") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  list(plot_resid = plot_resid, plot_fit = plot_fit)
}


#' Graphiques résidus et simulation statique (version interactive)
#'
#' Variante de \code{\link{make_plot_estim}} produisant des graphiques interactifs
#' via \pkg{ggiraph}, incluant un graphique en niveau et le thème OFCE.
#'
#' @param estim objet \code{lm} issu de l'estimation
#' @param data data.frame contenant au moins une colonne \code{date} et toutes
#'   les variables de l'équation
#' @return liste avec \code{plot_resid}, \code{plot_fit} (delta log) et
#'   \code{plot_fit_niv} (niveau)
#' @export
make_plot_estim2 <- function(estim, data) {
  lignes_utilisees <- as.numeric(rownames(estim$model))
  temps_estim <- data$date[lignes_utilisees]

  endog.value <- estim[["model"]][[1]]
  residus <- residuals(estim)
  prediction <- fitted(estim)

  endog_niveau.name <- all.vars(formula(estim))[1]
  endog_niveau.value <- data[[endog_niveau.name]][lignes_utilisees]
  ylag <- lag(
    log(data[[endog_niveau.name]][(min(lignes_utilisees) - 1):max(lignes_utilisees)]),
    1
  )[-1]
  prediction_niveau <- exp(prediction + ylag)

  graphic.data <- cbind(
    date = temps_estim,
    data.frame(
      endog = endog.value,
      residus = residus,
      prediction = prediction,
      endog_niveau = endog_niveau.value,
      prediction_niveau = prediction_niveau
    )
  )
  graphic.data <- dplyr::mutate(
    graphic.data,
    tooltip_resid = glue::glue("<b>{date}</b><br>Residu : {round(residus, 3)}"),
    tooltip_pred  = glue::glue("<b>{date}</b><br>Simulation : {round(prediction * 100, 2)}%"),
    tooltip_endog = glue::glue("<b>{date}</b><br>Observe : {round(endog * 100, 2)}%")
  )

  plot_resid <- ggplot2::ggplot(graphic.data, ggplot2::aes(x = date, y = residus)) +
    ggiraph::geom_bar_interactive(ggplot2::aes(tooltip = tooltip_resid),
      stat = "identity", color = "blue"
    ) +
    ggplot2::ggtitle("Residus de l'estimation") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ofce::theme_ofce() +
    ofce::scale_ofce_date(date_breaks = "1 years") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = NULL)

  plot_fit <- ggplot2::ggplot(graphic.data, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = endog, color = "Observe")) +
    ggplot2::geom_line(ggplot2::aes(y = prediction, color = "Simulation")) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = endog, tooltip = tooltip_endog, data_id = date, fill = "Observe"),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = prediction, tooltip = tooltip_pred, data_id = date, fill = "Simulation"),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggplot2::ggtitle(paste0("D1.log(", endog_niveau.name, ") : Observations / Simulation statique")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ofce::theme_ofce() +
    ofce::scale_ofce_date(date_breaks = "1 years") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = NULL, y = NULL, color = NULL) +
    ggplot2::scale_color_manual(
      values = c("Observe" = "blue", "Simulation" = "red"),
      aesthetics = c("fill", "color")
    ) +
    ggplot2::guides(fill = "none")

  plot_fit_niv <- ggplot2::ggplot(graphic.data, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = endog_niveau, color = "Observe")) +
    ggplot2::geom_line(ggplot2::aes(y = prediction_niveau, color = "Simulation")) +
    ggplot2::ggtitle(paste0(endog_niveau.name, ": Observations / Simulation statique")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ofce::theme_ofce() +
    ofce::scale_ofce_date(date_breaks = "1 years") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = NULL, y = NULL, color = NULL) +
    ggplot2::scale_color_manual(values = c("Observe" = "blue", "Simulation" = "red"))

  list(plot_resid = plot_resid, plot_fit = plot_fit, plot_fit_niv = plot_fit_niv)
}


#' Graphique de simulation dynamique
#'
#' Produit un graphique interactif comparant la série observée à la simulation
#' dynamique calculée par \code{\link{simulation_dynamique}}.
#' Les indicatrices de la forme \code{i[annee]q[trimestre]} sont représentées
#' par des bandes grises verticales.
#'
#' @param estim objet \code{lm} issu de l'estimation du MCE
#' @param data data.frame contenant au moins une colonne \code{date} et toutes
#'   les variables de l'équation
#' @param debut_graph première date affichée ; si \code{NULL}, première date sans NA
#' @param debut_simul date de début de la simulation ; si \code{NULL}, première date sans NA
#' @return objet \code{ggplot} interactif
#' @export
make_plot_simul_dynamique <- function(estim, data, debut_graph = NULL, debut_simul = NULL) {
  vars <- all.vars(formula(estim))
  endog <- vars[1]

  if (is.null(debut_simul)) debut_simul <- debut_graph
  data_dynam <- simulation_dynamique(
    estim = estim, data = data,
    debut_graph = debut_graph, debut_simul = debut_simul
  )

  data_dynam <- dplyr::mutate(
    data_dynam,
    observe_growth = (observe - dplyr::lag(observe, 1)) / dplyr::lag(observe, 1),
    simul_growth   = (simul_dynamique - dplyr::lag(simul_dynamique, 1)) / dplyr::lag(simul_dynamique, 1),
    tooltip_obs = glue::glue(
      "<b>{date}</b><br> {endog} observe : {observe}<br> Croissance {endog} observe : {round(observe_growth * 100, 1)}%"
    ),
    tooltip_sim = glue::glue(
      "<b>{date}</b><br> {endog} simule : {round(simul_dynamique, 0)}<br> Residu : {round(residu, 0)}<br> Croissance {endog} simule : {round(simul_growth * 100, 1)}%"
    )
  )

  indic <- vars[grepl("^i[0-9]{4}q[1-4]$", vars)]
  if (length(indic) > 0) {
    annee_indic <- as.numeric(substr(indic, 2, 5))
    trim_indic  <- as.numeric(substr(indic, 7, 7))
    date_indic  <- as.Date(paste(annee_indic, (trim_indic - 1) * 3 + 1, "01", sep = "-"))
    date_indic  <- date_indic[date_indic > dplyr::first(data_dynam$date)]
    rects <- data.frame(
      xmin    = lubridate::`%m+%`(date_indic, months(-1)) - lubridate::days(15),
      xmax    = lubridate::`%m+%`(date_indic, months(1))  + lubridate::days(15),
      ymin    = -Inf,
      ymax    = Inf,
      tooltip = "Indicatrice"
    )
  } else {
    rects <- data.frame(
      xmin = numeric(0), xmax = numeric(0),
      ymin = numeric(0), ymax = numeric(0),
      tooltip = character(0)
    )
  }

  ggplot2::ggplot(data_dynam, ggplot2::aes(x = date)) +
    ggiraph::geom_rect_interactive(
      data = rects,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, tooltip = tooltip),
      inherit.aes = FALSE, fill = "grey95"
    ) +
    ggplot2::geom_line(ggplot2::aes(y = observe, color = "Observe")) +
    ggplot2::geom_line(ggplot2::aes(y = simul_dynamique, color = "Simulation dynamique")) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = observe, tooltip = tooltip_obs, data_id = date, fill = "Observe"),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = simul_dynamique, tooltip = tooltip_sim, data_id = date, fill = "Simulation dynamique"),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggplot2::ggtitle(paste0(endog, ": Observations / Simulation dynamique")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ofce::theme_ofce() +
    ofce::scale_ofce_date(date_breaks = "2 years") +
    ggplot2::labs(x = NULL, y = NULL, color = NULL) +
    ggplot2::scale_color_manual(
      values = c("Observe" = "blue", "Simulation dynamique" = "red"),
      aesthetics = c("fill", "color")
    ) +
    ggplot2::guides(fill = "none")
}
