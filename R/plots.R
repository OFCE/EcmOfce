# Utilitaire interne : formate une date en "YYYYTq"
.date_trim <- function(date) {
  paste0(lubridate::year(date), "T", lubridate::quarter(date))
}

# Utilitaire interne : construit le data.frame des rectangles d'indicatrices
.build_rects <- function(indic, debut_graph) {
  if (length(indic) == 0) return(NULL)
  annee <- as.numeric(substr(indic, 2, 5))
  trim  <- as.numeric(substr(indic, 7, 7))
  dates <- as.Date(paste(annee, (trim - 1) * 3 + 1, "01", sep = "-"))
  dates <- dates[dates > debut_graph]
  if (length(dates) == 0) return(NULL)
  data.frame(
    xmin    = lubridate::`%m+%`(dates, months(-1)) - lubridate::days(15),
    xmax    = lubridate::`%m+%`(dates, months(1))  + lubridate::days(15),
    ymin    = -Inf,
    ymax    = Inf,
    tooltip = paste0("Indicatrice au ", .date_trim(dates))
  )
}

# Utilitaire interne : couche ggiraph pour les rectangles d'indicatrices
.indic_layer <- function(rects) {
  if (is.null(rects)) return(NULL)
  ggiraph::geom_rect_interactive(
    data = rects,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, tooltip = tooltip),
    inherit.aes = FALSE, fill = "grey95"
  )
}

# Utilitaire interne : zone grisee de prevision (remplace ofce::annotate_prevision)
.prev_layer <- function(debut_prev) {
  ggplot2::annotate(
    "rect",
    xmin = as.Date(debut_prev), xmax = as.Date("2200-01-01"),
    ymin = -Inf, ymax = Inf,
    fill = "grey80", alpha = 0.2
  )
}


#' Graphiques résidus et simulation statique (version simple)
#'
#' Représente la série observée vs la série simulée statiquement, et les résidus.
#' Les dates utilisées proviennent de la colonne \code{date} de \code{data}.
#'
#' @param estim objet \code{lm} issu de l'estimation
#' @param data data.frame contenant au moins une colonne \code{date}
#' @return liste avec \code{plot_resid} et \code{plot_fit}
#' @export
make_plot_estim <- function(estim, data) {
  sample_length <- as.numeric(length(estim[["model"]][[1]]))
  lignes_utilisees <- as.numeric(rownames(estim$model))
  temps_estim <- data[["date"]][lignes_utilisees]

  endog.value <- estim[["model"]][[1]]
  residus    <- residuals(estim)
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
  residus    <- residuals(estim)
  prediction <- fitted(estim)

  endog_niveau.name  <- all.vars(formula(estim))[1]
  endog_niveau.value <- data[[endog_niveau.name]][lignes_utilisees]
  ylag <- lag(
    log(data[[endog_niveau.name]][(min(lignes_utilisees) - 1):max(lignes_utilisees)]),
    1
  )[-1]
  prediction_niveau <- exp(prediction + ylag)

  graphic.data <- cbind(
    date = temps_estim,
    data.frame(
      endog             = endog.value,
      residus           = residus,
      prediction        = prediction,
      endog_niveau      = endog_niveau.value,
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


#' Dynamic simulation plots: level, quarterly growth and annual change
#'
#' Produces three interactive plots comparing the observed series to the dynamic
#' simulation from \code{\link{simulation_dynamique}}: in level, in quarterly
#' growth, and in annual change. Each plot is a \pkg{patchwork} assembly of the
#' main panel and the residual panel. Dummy variables of the form
#' \code{i[year]q[quarter]} are shown as grey vertical bands.
#'
#' @param estim \code{lm} object from MCE estimation
#' @param data data.frame with at least a \code{date} column and all equation variables
#' @param debut_graph first date displayed; if \code{NULL}, first non-NA date
#' @param debut_simul simulation start date; if \code{NULL}, same as \code{debut_graph}
#' @param debut_prev forecast start date (greyed area); if \code{NULL} (default), no shading
#' @param col_observe colour for observed series (default: \code{"#003189"})
#' @param col_simul colour for simulated series (default: \code{"#e75c58"})
#' @return list with \code{plot_niveau}, \code{plot_g_trim} and \code{plot_g_an}
#' @export
make_plot_simul_dynamique <- function(estim, data, debut_graph = NULL, debut_simul = NULL,
                                       debut_prev = NULL,
                                       col_observe = "#003189", col_simul = "#e75c58") {
  vars  <- all.vars(formula(estim))
  endog <- vars[1]

  if (is.null(debut_simul)) debut_simul <- debut_graph
  data_dynam <- simulation_dynamique(
    estim = estim, data = data,
    debut_graph = debut_graph, debut_simul = debut_simul
  )

  data_dynam <- dplyr::mutate(data_dynam,
    residu        = round(observe - simul_dynamique, 10),
    observe_g_trim = (observe - dplyr::lag(observe, 1)) / dplyr::lag(observe, 1),
    simul_g_trim   = (simul_dynamique - dplyr::lag(simul_dynamique, 1)) / dplyr::lag(simul_dynamique, 1),
    residu_g_trim  = round(observe_g_trim - simul_g_trim, 10),
    observe_g_an   = (observe - dplyr::lag(observe, 4)) / dplyr::lag(observe, 4),
    simul_g_an     = (simul_dynamique - dplyr::lag(simul_dynamique, 4)) / dplyr::lag(simul_dynamique, 4),
    residu_g_an    = round(observe_g_an - simul_g_an, 10)
  )
  data_dynam <- dplyr::select(data_dynam, date,
    dplyr::starts_with("observe"), dplyr::starts_with("simul"), dplyr::starts_with("residu")
  )

  if (is.null(debut_graph)) debut_graph <- dplyr::first(data_dynam$date)

  rects     <- .build_rects(vars[grepl("^i[0-9]{4}q[1-4]$", vars)], debut_graph)
  has_prev  <- !is.null(debut_prev) && max(data_dynam$date) > debut_prev

  # ── Plot 1 : Niveau ────────────────────────────────────────────────────────
  graph_niv <- dplyr::mutate(
    dplyr::select(data_dynam, date, observe, simul_dynamique, residu, observe_g_trim, simul_g_trim),
    var          = dplyr::case_when(residu > 0 ~ "Observe", residu < 0 ~ "Simule", TRUE ~ NA_character_),
    tooltip_obs  = glue::glue("<b>{.date_trim(date)}</b><br>Observe : {fmt_val(observe, 0)}<br>Croissance trim. : {fmt_val(observe_g_trim * 100, 1)}%"),
    tooltip_sim  = glue::glue("<b>{.date_trim(date)}</b><br>Simule : {fmt_val(simul_dynamique, 0)}<br>Croissance trim. : {fmt_val(simul_g_trim * 100, 1)}%"),
    tooltip_resid = glue::glue("<b>{.date_trim(date)}</b><br>Residu : {fmt_val(residu, 0)}")
  )

  g_resid_niv <- ggplot2::ggplot(graph_niv) +
    { if (has_prev) .prev_layer(debut_prev) } +
    .indic_layer(rects) +
    ggbraid::geom_braid(
      ggplot2::aes(x = date, ymin = 0, ymax = residu,
        fill = factor(residu > 0, levels = c(TRUE, FALSE)),
        color = factor(residu > 0, levels = c(TRUE, FALSE))),
      alpha = 0.2, show.legend = FALSE, linewidth = 0.2
    ) +
    ggplot2::scale_color_manual(
      values = stats::setNames(c(col_observe, col_simul), c("TRUE", "FALSE")),
      aesthetics = c("fill", "color")
    ) +
    ggiraph::geom_point_interactive(
      data = ~ .x[!is.na(.x$residu) & .x$residu >= 0, ],
      ggplot2::aes(x = date, y = residu, tooltip = tooltip_resid, data_id = date),
      fill = col_observe, color = "white", shape = 21, size = 1.5, hover_nearest = TRUE
    ) +
    ggiraph::geom_point_interactive(
      data = ~ .x[!is.na(.x$residu) & .x$residu < 0, ],
      ggplot2::aes(x = date, y = residu, tooltip = tooltip_resid, data_id = date),
      fill = col_simul, color = "white", shape = 21, size = 1.5, hover_nearest = TRUE
    ) +
    ggplot2::labs(y = NULL) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(2)) +
    ofce::scale_ofce_date(date_breaks = "2 years") +
    ofce::theme_ofce(plot.margin = ggplot2::margin(t = 0))

  g_var_niv <- ggplot2::ggplot(
    dplyr::filter(graph_niv, !is.na(simul_dynamique)), ggplot2::aes(x = date)
  ) +
    { if (has_prev) .prev_layer(debut_prev) } +
    .indic_layer(rects) +
    ggbraid::geom_braid(ggplot2::aes(ymin = simul_dynamique, ymax = observe, fill = var), color = NA, alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = simul_dynamique, color = "Simule")) +
    ggplot2::geom_line(ggplot2::aes(y = observe, color = "Observe")) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = simul_dynamique, tooltip = tooltip_sim, data_id = date, fill = "Simule"),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = observe, tooltip = tooltip_obs, data_id = date, fill = "Observe"),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = c("Observe" = col_observe, "Simule" = col_simul),
      aesthetics = c("fill", "color")
    ) +
    ofce::scale_ofce_date(date_breaks = "2 years") +
    ggplot2::labs(x = NULL, y = "en niveau", color = NULL, fill = NULL) +
    ggplot2::guides(fill = "none") +
    ggplot2::ggtitle(paste0(endog, ": Observations / Simulation dynamique")) +
    ofce::theme_ofce(plot.margin = ggplot2::margin(b = 1))

  plot_niveau <- patchwork::wrap_plots(g_var_niv, g_resid_niv, ncol = 1, heights = c(14/17, 3/17)) +
    patchwork::plot_layout(axes = "collect_x")

  # ── Plot 2 : Croissance trimestrielle ──────────────────────────────────────
  graph_gt <- dplyr::mutate(
    dplyr::select(data_dynam, date, observe_g_trim, simul_g_trim, residu_g_trim),
    var          = dplyr::case_when(residu_g_trim > 0 ~ "Observe", residu_g_trim < 0 ~ "Simule", TRUE ~ NA_character_),
    tooltip_obs  = glue::glue("<b>{.date_trim(date)}</b><br>Observe : {fmt_val(observe_g_trim * 100, 1)}%"),
    tooltip_sim  = glue::glue("<b>{.date_trim(date)}</b><br>Simule : {fmt_val(simul_g_trim * 100, 1)}%"),
    tooltip_resid = glue::glue("<b>{.date_trim(date)}</b><br>Residu : {fmt_val(residu_g_trim * 100, 1)}%")
  )

  g_resid_gt <- ggplot2::ggplot(graph_gt) +
    { if (has_prev) .prev_layer(debut_prev) } +
    .indic_layer(rects) +
    ggbraid::geom_braid(
      ggplot2::aes(x = date, ymin = 0, ymax = residu_g_trim,
        fill = residu_g_trim > 0, color = residu_g_trim > 0),
      alpha = 0.2, show.legend = FALSE, linewidth = 0.2
    ) +
    ggplot2::scale_color_manual(values = c(col_simul, col_observe), aesthetics = c("fill", "color")) +
    ggiraph::geom_point_interactive(
      data = ~ .x[!is.na(.x$residu_g_trim) & .x$residu_g_trim >= 0, ],
      ggplot2::aes(x = date, y = residu_g_trim, tooltip = tooltip_resid, data_id = date),
      fill = col_observe, color = "white", shape = 21, size = 1.5, hover_nearest = TRUE
    ) +
    ggiraph::geom_point_interactive(
      data = ~ .x[!is.na(.x$residu_g_trim) & .x$residu_g_trim < 0, ],
      ggplot2::aes(x = date, y = residu_g_trim, tooltip = tooltip_resid, data_id = date),
      fill = col_simul, color = "white", shape = 21, size = 1.5, hover_nearest = TRUE
    ) +
    ggplot2::labs(y = NULL) +
    ggplot2::scale_y_continuous(
      limits = function(lims) c(min(lims[1], -0.02), max(lims[2], 0.02)),
      breaks = scales::pretty_breaks(2),
      labels = scales::percent_format(accuracy = 1)
    ) +
    ofce::scale_ofce_date(date_breaks = "2 years") +
    ofce::theme_ofce(plot.margin = ggplot2::margin(t = 0))

  g_var_gt <- ggplot2::ggplot(
    dplyr::filter(graph_gt, !is.na(simul_g_trim)), ggplot2::aes(x = date)
  ) +
    { if (has_prev) .prev_layer(debut_prev) } +
    .indic_layer(rects) +
    ggbraid::geom_braid(ggplot2::aes(ymin = simul_g_trim, ymax = observe_g_trim, fill = var), color = NA, alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = simul_g_trim, color = "Simule")) +
    ggplot2::geom_line(ggplot2::aes(y = observe_g_trim, color = "Observe")) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = simul_g_trim, tooltip = tooltip_sim, data_id = date, fill = "Simule"),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = observe_g_trim, tooltip = tooltip_obs, data_id = date, fill = "Observe"),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = c("Observe" = col_observe, "Simule" = col_simul),
      aesthetics = c("fill", "color")
    ) +
    ofce::scale_ofce_date(date_breaks = "2 years") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(x = NULL, y = "Croissance trimestrielle (en %)", color = NULL, fill = NULL) +
    ggplot2::guides(fill = "none") +
    ggplot2::ggtitle(paste0(endog, ", % trim: Observations / Simulation dynamique")) +
    ofce::theme_ofce(plot.margin = ggplot2::margin(b = 1))

  plot_g_trim <- patchwork::wrap_plots(g_var_gt, g_resid_gt, ncol = 1, heights = c(14/17, 3/17)) +
    patchwork::plot_layout(axes = "collect_x")

  # ── Plot 3 : Glissement annuel ─────────────────────────────────────────────
  graph_ga <- dplyr::mutate(
    dplyr::select(data_dynam, date, observe_g_an, simul_g_an, residu_g_an),
    var          = dplyr::case_when(residu_g_an > 0 ~ "Observe", residu_g_an < 0 ~ "Simule", TRUE ~ NA_character_),
    tooltip_obs  = glue::glue("<b>{.date_trim(date)}</b><br>Observe : {fmt_val(observe_g_an * 100, 1)}%"),
    tooltip_sim  = glue::glue("<b>{.date_trim(date)}</b><br>Simule : {fmt_val(simul_g_an * 100, 1)}%"),
    tooltip_resid = glue::glue("<b>{.date_trim(date)}</b><br>Residu : {fmt_val(residu_g_an * 100, 1)}%")
  )

  g_resid_ga <- ggplot2::ggplot(graph_ga) +
    { if (has_prev) .prev_layer(debut_prev) } +
    .indic_layer(rects) +
    ggbraid::geom_braid(
      ggplot2::aes(x = date, ymin = 0, ymax = residu_g_an,
        fill = residu_g_an > 0, color = residu_g_an > 0),
      alpha = 0.2, show.legend = FALSE, linewidth = 0.2
    ) +
    ggplot2::scale_color_manual(values = c(col_simul, col_observe), aesthetics = c("fill", "color")) +
    ggiraph::geom_point_interactive(
      data = ~ .x[!is.na(.x$residu_g_an) & .x$residu_g_an >= 0, ],
      ggplot2::aes(x = date, y = residu_g_an, tooltip = tooltip_resid, data_id = date),
      fill = col_observe, color = "white", shape = 21, size = 1.5, hover_nearest = TRUE
    ) +
    ggiraph::geom_point_interactive(
      data = ~ .x[!is.na(.x$residu_g_an) & .x$residu_g_an < 0, ],
      ggplot2::aes(x = date, y = residu_g_an, tooltip = tooltip_resid, data_id = date),
      fill = col_simul, color = "white", shape = 21, size = 1.5, hover_nearest = TRUE
    ) +
    ggplot2::labs(y = NULL) +
    ggplot2::scale_y_continuous(
      limits = function(lims) c(min(lims[1], -0.02), max(lims[2], 0.02)),
      breaks = scales::pretty_breaks(2),
      labels = scales::percent_format(accuracy = 1)
    ) +
    ofce::scale_ofce_date(date_breaks = "2 years") +
    ofce::theme_ofce(plot.margin = ggplot2::margin(t = 0))

  g_var_ga <- ggplot2::ggplot(
    dplyr::filter(graph_ga, !is.na(simul_g_an)), ggplot2::aes(x = date)
  ) +
    { if (has_prev) .prev_layer(debut_prev) } +
    .indic_layer(rects) +
    ggbraid::geom_braid(ggplot2::aes(ymin = simul_g_an, ymax = observe_g_an, fill = var), color = NA, alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = simul_g_an, color = "Simule")) +
    ggplot2::geom_line(ggplot2::aes(y = observe_g_an, color = "Observe")) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = simul_g_an, tooltip = tooltip_sim, data_id = date, fill = "Simule"),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = observe_g_an, tooltip = tooltip_obs, data_id = date, fill = "Observe"),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = c("Observe" = col_observe, "Simule" = col_simul),
      aesthetics = c("fill", "color")
    ) +
    ofce::scale_ofce_date(date_breaks = "2 years") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(x = NULL, y = "Glissement annuel (en %)", color = NULL, fill = NULL) +
    ggplot2::guides(fill = "none") +
    ggplot2::ggtitle(paste0(endog, ", % an: Observations / Simulation dynamique")) +
    ofce::theme_ofce(plot.margin = ggplot2::margin(b = 1))

  plot_g_an <- patchwork::wrap_plots(g_var_ga, g_resid_ga, ncol = 1, heights = c(14/17, 3/17)) +
    patchwork::plot_layout(axes = "collect_x")

  list(plot_niveau = plot_niveau, plot_g_trim = plot_g_trim, plot_g_an = plot_g_an)
}


#' Comparaison de plusieurs simulations dynamiques
#'
#' Superpose plusieurs simulations dynamiques sur un même graphique, soit en
#' faisant varier l'équation estimée (\code{estim_list}), soit en faisant varier
#' les données (\code{data_list}). Retourne un graphique en niveau et un en
#' croissance trimestrielle, chacun avec un panneau de résidus par simulation.
#'
#' @param estim_list liste nommée d'objets \code{lm}, ou un seul objet \code{lm}
#'   si \code{data_list} est une liste nommée
#' @param data_list liste nommée de data.frames, ou un seul data.frame si
#'   \code{estim_list} est une liste nommée
#' @param debut_graph première date affichée ; si \code{NULL}, première date sans NA
#' @param debut_simul date de début de la simulation ; si \code{NULL}, égale à
#'   \code{debut_graph}
#' @param debut_prev date de début de la prévision pour l'annotation ; si
#'   \code{NULL} (défaut), pas d'annotation
#' @return liste avec \code{plot_niveau} et \code{plot_g_trim}
#' @export
make_plot_comparaison_simul <- function(estim_list, data_list,
                                         debut_graph = NULL, debut_simul = NULL,
                                         debut_prev = NULL) {
  if (is.null(debut_simul)) debut_simul <- debut_graph

  if (!is.object(estim_list) && !is.null(names(estim_list))) {
    nb_courbes <- length(estim_list)
    nom_courbes <- names(estim_list)
    vars <- all.vars(formula(estim_list[[1]]))
    data_dynam <- dplyr::bind_rows(purrr::imap(estim_list, function(estim, nom) {
      dplyr::mutate(
        simulation_dynamique(estim = estim, data = data_list,
                             debut_graph = debut_graph, debut_simul = debut_simul),
        courbe = nom
      )
    }))
  } else if (!is.object(data_list) && !is.null(names(data_list))) {
    nb_courbes <- length(data_list)
    nom_courbes <- names(data_list)
    vars <- all.vars(formula(estim_list))
    data_dynam <- dplyr::bind_rows(purrr::imap(data_list, function(data, nom) {
      dplyr::mutate(
        simulation_dynamique(estim = estim_list, data = data,
                             debut_graph = debut_graph, debut_simul = debut_simul),
        courbe = nom
      )
    }))
  } else {
    stop("Fournir soit estim_list (liste nommee), soit data_list (liste nommee).")
  }
  data_dynam <- dplyr::select(data_dynam, date, courbe, observe, simul_dynamique, residu, dplyr::everything())

  data_dynam <- dplyr::group_by(data_dynam, courbe)
  data_dynam <- dplyr::mutate(data_dynam,
    residu        = round(observe - simul_dynamique, 10),
    observe_g_trim = (observe - dplyr::lag(observe, 1)) / dplyr::lag(observe, 1),
    simul_g_trim   = (simul_dynamique - dplyr::lag(simul_dynamique, 1)) / dplyr::lag(simul_dynamique, 1),
    residu_g_trim  = round(observe_g_trim - simul_g_trim, 10),
    tooltip_obs          = glue::glue("<b>{.date_trim(date)}</b><br>Observe : {fmt_val(observe, 0)}"),
    tooltip_sim          = glue::glue("<b>{.date_trim(date)}</b><br>Simule ({courbe}) : {fmt_val(simul_dynamique, 0)}"),
    tooltip_resid        = glue::glue("<b>{.date_trim(date)}</b><br>Residu ({courbe}) : {fmt_val(residu, 0)}"),
    tooltip_sim_gt       = glue::glue("<b>{.date_trim(date)}</b><br>Simule ({courbe}) : {fmt_val(simul_g_trim * 100, 1)}%"),
    tooltip_obs_gt       = glue::glue("<b>{.date_trim(date)}</b><br>Observe : {fmt_val(observe_g_trim * 100, 1)}%"),
    tooltip_resid_gt     = glue::glue("<b>{.date_trim(date)}</b><br>Residu ({courbe}) : {fmt_val(residu_g_trim * 100, 1)}%")
  )
  data_dynam <- dplyr::ungroup(data_dynam)

  if (is.null(debut_graph)) debut_graph <- dplyr::first(data_dynam$date)
  rects    <- .build_rects(vars[grepl("^i[0-9]{4}q[1-4]$", vars)], debut_graph)
  has_prev <- !is.null(debut_prev) && max(data_dynam$date) > debut_prev

  pal_simul <- stats::setNames(scales::hue_pal()(nb_courbes), nom_courbes)
  pal_full  <- c("Observe" = "grey30", pal_simul)

  # ── Niveau ─────────────────────────────────────────────────────────────────
  g_var_niv <- ggplot2::ggplot(data_dynam, ggplot2::aes(x = date)) +
    { if (has_prev) .prev_layer(debut_prev) } +
    .indic_layer(rects) +
    ggplot2::geom_line(ggplot2::aes(y = simul_dynamique, color = courbe)) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = simul_dynamique, tooltip = tooltip_sim,
        data_id = paste(date, courbe), fill = courbe),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggplot2::geom_line(ggplot2::aes(y = observe, color = "Observe")) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = observe, tooltip = tooltip_obs, data_id = date, fill = "Observe"),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggplot2::scale_color_manual(values = pal_full, aesthetics = c("fill", "color"),
      breaks = c("Observe", nom_courbes)) +
    ofce::scale_ofce_date(date_breaks = "2 years") +
    ggplot2::labs(x = NULL, y = "en niveau", color = NULL, fill = NULL) +
    ggplot2::guides(fill = "none") +
    ggplot2::ggtitle(paste0(vars[1], ": Observations / Simulation dynamique")) +
    ofce::theme_ofce(plot.margin = ggplot2::margin(b = 1))

  plots_resid_niv <- purrr::imap(nom_courbes, function(nom, i) {
    df  <- dplyr::filter(data_dynam, courbe == nom)
    col <- pal_simul[[nom]]
    ggplot2::ggplot(df, ggplot2::aes(x = date)) +
      { if (has_prev) .prev_layer(debut_prev) } +
      .indic_layer(rects) +
      ggbraid::geom_braid(
        ggplot2::aes(ymin = 0, ymax = residu, fill = residu > 0, color = residu > 0),
        alpha = 0.2, show.legend = FALSE, linewidth = 0.2
      ) +
      ggplot2::scale_color_manual(
        values = stats::setNames(c(col, scales::alpha(col, 0.4)), c("TRUE", "FALSE")),
        aesthetics = c("fill", "color")
      ) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(y = residu, tooltip = tooltip_resid, data_id = paste(date, courbe)),
        fill = col, color = "white", shape = 21, size = 1.2, hover_nearest = TRUE
      ) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(2)) +
      ofce::scale_ofce_date(date_breaks = "2 years") +
      ggplot2::labs(y = nom, x = NULL) +
      ofce::theme_ofce(plot.margin = ggplot2::margin(t = 0))
  })

  plot_niveau <- patchwork::wrap_plots(
    c(list(g_var_niv), plots_resid_niv),
    ncol = 1,
    heights = c(14/17, rep((3 / nb_courbes) / 17, nb_courbes))
  ) + patchwork::plot_layout(axes = "collect_x")

  # ── Croissance trimestrielle ───────────────────────────────────────────────
  g_var_gt <- ggplot2::ggplot(
    dplyr::filter(data_dynam, !is.na(simul_g_trim)), ggplot2::aes(x = date)
  ) +
    { if (has_prev) .prev_layer(debut_prev) } +
    .indic_layer(rects) +
    ggplot2::geom_line(ggplot2::aes(y = simul_g_trim, color = courbe)) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = simul_g_trim, tooltip = tooltip_sim_gt,
        data_id = paste(date, courbe), fill = courbe),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggplot2::geom_line(ggplot2::aes(y = observe_g_trim, color = "Observe")) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(y = observe_g_trim, tooltip = tooltip_obs_gt, data_id = date, fill = "Observe"),
      shape = 21, size = 1.5, color = "white", hover_nearest = TRUE
    ) +
    ggplot2::scale_color_manual(values = pal_full, aesthetics = c("fill", "color"),
      breaks = c("Observe", nom_courbes)) +
    ofce::scale_ofce_date(date_breaks = "2 years") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(x = NULL, y = "Croissance trimestrielle (en %)", color = NULL, fill = NULL) +
    ggplot2::guides(fill = "none") +
    ggplot2::ggtitle(paste0(vars[1], ", % trim: Observations / Simulation dynamique")) +
    ofce::theme_ofce(plot.margin = ggplot2::margin(b = 1))

  plots_resid_gt <- purrr::imap(nom_courbes, function(nom, i) {
    df  <- dplyr::filter(data_dynam, courbe == nom)
    col <- pal_simul[[nom]]
    ggplot2::ggplot(df, ggplot2::aes(x = date)) +
      { if (has_prev) .prev_layer(debut_prev) } +
      .indic_layer(rects) +
      ggbraid::geom_braid(
        ggplot2::aes(ymin = 0, ymax = residu_g_trim, fill = residu_g_trim > 0, color = residu_g_trim > 0),
        alpha = 0.2, show.legend = FALSE, linewidth = 0.2
      ) +
      ggplot2::scale_color_manual(
        values = stats::setNames(c(col, scales::alpha(col, 0.4)), c("TRUE", "FALSE")),
        aesthetics = c("fill", "color")
      ) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(y = residu_g_trim, tooltip = tooltip_resid_gt, data_id = paste(date, courbe)),
        fill = col, color = "white", shape = 21, size = 1.2, hover_nearest = TRUE
      ) +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(2),
        labels = scales::percent_format(accuracy = 1)
      ) +
      ofce::scale_ofce_date(date_breaks = "2 years") +
      ggplot2::labs(y = nom, x = NULL) +
      ofce::theme_ofce(plot.margin = ggplot2::margin(t = 0))
  })

  plot_g_trim <- patchwork::wrap_plots(
    c(list(g_var_gt), plots_resid_gt),
    ncol = 1,
    heights = c(14/17, rep((3 / nb_courbes) / 17, nb_courbes))
  ) + patchwork::plot_layout(axes = "collect_x")

  list(plot_niveau = plot_niveau, plot_g_trim = plot_g_trim)
}
