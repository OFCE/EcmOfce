#' Données pour l'estimation récursive d'un MCE
#'
#' Pour chaque date de la séquence \code{[start, end]}, estime le modèle sur
#' la sous-période correspondante (en faisant varier la date de fin si
#' \code{date_debut_fixe = TRUE}, la date de début sinon) et stocke les
#' coefficients et t-statistiques. Les coefficients de long terme sont divisés
#' par la force de rappel (élasticité de long terme). Les estimations sur moins
#' de 40 trimestres sont ignorées.
#'
#' @param data data.frame contenant les données
#' @param model objet \code{formula} du MCE
#' @param start première date de la séquence récursive (chaîne "AAAA-MM-JJ")
#' @param end dernière date de la séquence récursive
#' @param date_debut_fixe si \code{TRUE} (défaut), la date de début est fixe et
#'   la date de fin varie ; si \code{FALSE}, la date de fin est fixe et la date
#'   de début varie
#' @param plot_var_ct si \code{TRUE}, toutes les variables (hors dummies) sont
#'   retournées ; si \code{FALSE} (défaut), seulement la force de rappel et les
#'   variables de long terme
#' @param dummies vecteur de noms d'indicatrices à gérer dynamiquement (ex.
#'   \code{c("i2020q1", "i2020q2")}) ; \code{NULL} par défaut
#' @return liste avec \code{df.param} et \code{df.tstat} (data.frames datés)
#' @export
data_recursif <- function(data, model, start, end,
                           date_debut_fixe = TRUE, plot_var_ct = FALSE,
                           dummies = NULL) {
  periode_recursif <- seq.Date(from = as.Date(start), to = as.Date(end), by = "quarter")

  dummies_presentes <- grep("^i\\d+q\\d+$", all.vars(model), value = TRUE)
  dummies <- unique(c(dummies_presentes, dummies))
  if (length(dummies_presentes) > 0) {
    remove_dummies   <- paste("-", dummies_presentes, collapse = " ")
    model_sans_dummy <- update(model, stats::as.formula(paste(". ~ .", remove_dummies)))
  } else {
    model_sans_dummy <- model
  }

  estim_sans_dummy <- lm(model_sans_dummy, data=data)
  vars <- names(estim_sans_dummy$coefficients)
  endog <- all.vars(stats::formula(estim_sans_dummy))[1]
  
  is_delta_log <- grepl("delta\\(1, log\\(", stats::formula(estim_sans_dummy)[2])
  if (isFALSE(is_delta_log)){
    stop("La variable dépendante n'est pas un delta(log(.))")
  }
  is_delta_delta_log <- grepl("delta\\(1, delta\\(1, log\\(", stats::formula(estim_sans_dummy)[2])
  if (is_delta_delta_log){
    stop("Variable endogène en delta(delta(log(.))) : cette fonction n'est pas adaptée, prendre dans le code fonctions_prix_salaires.R")
  }
  
  check_presence_var_ecart_lt <- any(grepl("ecart",vars) & grepl("lt",vars))
  var_lt <- grep("^(?!.*delta).*lag.*$", vars, value = TRUE, perl = TRUE) # var de LT avec lag() et pas delta
  var_ct <- setdiff(vars, var_lt)
  if (isTRUE(check_presence_var_ecart_lt)){
    var_force_rappel <- vars[grep("ecart", vars)]
    message(glue("Le coefficient de la variable {var_force_rappel} est la force de rappel"))
  }else{
    var_force_rappel <- var_lt[grepl(endog, var_lt)]
  }
  var_lt <- setdiff(var_lt, var_force_rappel)
  
  df.param <- data.frame(date = periode_recursif)
  df.tstat <- data.frame(date = periode_recursif)
  df.param[c(vars,dummies)] <- NA
  df.tstat[c(vars,dummies)] <- NA
  
  if (date_debut_fixe) {
    ### Cas ou on fait bouger la date de fin d'estimation ###
    for (i in seq_along(periode_recursif)) {
      date_fin <- periode_recursif[i]
      data_sub <- dplyr::filter(data, date <= date_fin)
      
      if (nrow(model.frame(model_sans_dummy, data = data_sub)) > 40) {
        dummies_period <- dummies[
          sapply(dummies, function(d) any(data_sub[[d]] == 1))
        ]
        model_final <- if (length(dummies_period) > 0) {
          update(model_sans_dummy, paste(". ~ . +", paste(dummies_period, collapse = " + ")))
        } else {
          model_sans_dummy
        }
        
        estim <- lm(model_final, data = data_sub)
        coefs <- estim$coefficients
        coefs[var_lt] <- coefs[var_lt] / (-coefs[var_force_rappel])
        tstat <- summary(estim)$coefficients[, "t value"]
        df.param[i, vars] <- coefs[vars]
        df.tstat[i, vars] <- tstat[vars]
      }
    }
  } else {
    ### Cas ou on fait bouger la date de debut d'estimation ###
    # ajoute quelques lignes en plus pour les retards
    df.param <- dplyr::bind_rows(
      df.param,
      data.frame(date = as.Date(end) %m+% months(3 * (1:4)))
    )
    df.tstat <- dplyr::bind_rows(
      df.tstat,
      data.frame(date = as.Date(end) %m+% months(3 * (1:4)))
    )
    
    for (i in seq_along(periode_recursif)) {
      date_debut <- periode_recursif[i]
      data_sub   <- dplyr::filter(data, date >= date_debut)
      
      if (nrow(model.frame(model_sans_dummy, data = data_sub)) > 40) {
        dummies_period <- dummies[
          sapply(dummies, function(d) any(data_sub[[d]] == 1))
        ]
        model_final <- if (length(dummies_period) > 0) {
          update(model_sans_dummy, paste(". ~ . +", paste(dummies_period, collapse = " + ")))
        } else {
          model_sans_dummy
        }
        
        estim <- lm(model_final, data = data_sub)
        i_avec_decalage  <- i + min(as.numeric(rownames(estim$model))) - 1
        coefs <- estim$coefficients
        coefs[var_lt] <- coefs[var_lt] / (-coefs[var_force_rappel])
        tstat <- summary(estim)$coefficients[, "t value"]
        df.param[i_avec_decalage, vars] <- coefs[vars]
        df.tstat[i_avec_decalage, vars] <- tstat[vars]
      }
    }
  } 
  
  cols_garder <- if (plot_var_ct) {
    !grepl("^i[0-9]{4}q[1-4]$", names(df.param))
  } else {
    names(df.param) %in% c("date", var_force_rappel, var_lt)
  }
  df.param <- df.param[, cols_garder]
  df.tstat <- df.tstat[, cols_garder]

  df.param <- dplyr::rename(df.param, `Force de rappel` = !!rlang::sym(var_force_rappel))
  df.tstat <- dplyr::rename(df.tstat, `Force de rappel` = !!rlang::sym(var_force_rappel))

  lignes_utilisees <- as.numeric(rownames(estim_sans_dummy$model))
  periodes_utilisees <- data$date[lignes_utilisees]
  df.param <- dplyr::filter(df.param, date %in% periodes_utilisees)
  df.tstat <- dplyr::filter(df.tstat, date %in% periodes_utilisees)
  
  return(list(df.param = df.param, df.tstat = df.tstat))
}


#' Graphique d'estimation récursive
#'
#' Pour chaque variable du modèle, représente l'évolution du coefficient
#' selon la période d'estimation (date de fin ou de début variable).
#' Les t-statistiques sont affichées dans le tooltip interactif.
#'
#' @param data data.frame contenant les données
#' @param model objet \code{formula} du MCE
#' @param start première date de la séquence récursive
#' @param end dernière date de la séquence récursive
#' @param date_debut_fixe si \code{TRUE} (défaut), fait varier la date de fin
#' @param plot_var_ct si \code{TRUE}, affiche toutes les variables (hors dummies)
#' @param dummies vecteur d'indicatrices à gérer dynamiquement ; \code{NULL} par défaut
#' @return objet \code{ggplot} interactif
#' @export
make_plot_recursif <- function(data, model, start, end,
                                date_debut_fixe = TRUE, plot_var_ct = FALSE,
                                dummies = NULL) {
  data_list <- data_recursif(
    data = data, model = model, start = start, end = end,
    date_debut_fixe = date_debut_fixe, plot_var_ct = plot_var_ct, dummies = dummies
  )
  df.param <- dplyr::filter(
    data_list$df.param,
    !is.na(`Force de rappel`)
  )
  df.tstat <- dplyr::filter(
    data_list$df.tstat,
    !is.na(`Force de rappel`)
  )
  
  # Rectangles pour les indicatrices (sur l'axe des dates récursives)
  rects <- NULL
  dummies_presentes <- grep("^i\\d+q\\d+$", all.vars(model), value = TRUE)
  dummies <- unique(c(dummies_presentes, dummies))
  if (length(dummies)>0) {
    annee_dummies     <- as.numeric(substr(dummies, 2, 5))
    trim_dummies      <- as.numeric(substr(dummies, 7, 7))
    date_dummies <- as.Date(paste(annee_dummies, (trim_dummies - 1) * 3 + 1, "01", sep = "-"))
    date_dummies <- date_dummies[date_dummies > dplyr::first(df.param$date)]
    if (length(date_dummies) > 0) {
      rects <- data.frame(
        xmin    = lubridate::`%m+%`(date_dummies, months(-1)) - lubridate::days(15),
        xmax    = lubridate::`%m+%`(date_dummies, months(1))  + lubridate::days(15),
        ymin    = -Inf,
        ymax    = Inf,
        tooltip = paste0("Indicatrice au ", .date_trim(date_dummies))
      )
    }
  }

  # Mise en forme longue
  df.long <- tidyr::pivot_longer(df.param, -date)
  tstat_long <- tidyr::pivot_longer(df.tstat, -date, values_to = "tstat")
  df.long <- dplyr::left_join(df.long, dplyr::select(tstat_long, date, name, tstat),
    by = c("date", "name")
  )
  df.long <- dplyr::mutate(
    df.long,
    delta = grepl("delta", name),
    tooltip = glue::glue(
      "<b>{.date_trim(date)}</b><br><b>{name}</b><br>Coefficient : {ofce::fmt_val(value, 3)}<br>T-stat : {ofce::fmt_val(tstat, 3)}"
    ),
    name = forcats::fct_relevel(name, "Force de rappel", after = 0)
  )

  ymin_lim <- max(min(df.long$value, na.rm = TRUE), -3)
  ymax_lim <- min(max(df.long$value, na.rm = TRUE),  3)

  titre <- if (date_debut_fixe) {
    lignes <- as.numeric(rownames(model.frame(model, data = data)))
    paste0("Estimation recursive, periode : ", .date_trim(min(data$date[lignes])), " - ...")
  } else {
    lignes <- as.numeric(rownames(model.frame(model, data = data)))
    paste0("Estimation recursive, periode : ... - ", .date_trim(max(data$date[lignes])))
  }

  p <- ggplot2::ggplot(data = df.long) +
    { if (!is.null(rects)) {
      ggiraph::geom_rect_interactive(
        data = rects,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, tooltip = tooltip),
        inherit.aes = FALSE, fill = "grey95"
      )
    }} +
    ggplot2::geom_line(ggplot2::aes(x = date, y = value, color = name, alpha = delta)) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(x = date, y = value, tooltip = tooltip, data_id = date, fill = name, alpha = delta),
      hover_nearest = TRUE, size = 1.5, shape = 21, color = "white"
    ) +
    ofce::theme_ofce() +
    ggplot2::theme(legend.position = "bottom") +
    ofce::scale_ofce_date(date_breaks = "2 years") +
    ggplot2::scale_y_continuous(
      breaks = seq(floor(ymin), ceiling(ymax), by = 1), 
      limits = c(ymin, ymax), 
      oob = scales::oob_squish
    ) +
    { if (plot_var_ct) {
        ggplot2::scale_alpha_manual(values = c("FALSE" = 1, "TRUE" = 0.3), guide = "none")
      } else {
        ggplot2::scale_alpha_manual(values = c("FALSE" = 1, "TRUE" = 1), guide = "none")
      }
    } +
    ggplot2::labs(y = NULL, x = NULL, colour = NULL) +
    ggplot2::guides(fill = "none", color = ggplot2::guide_legend(ncol = 3)) +
    ggplot2::ggtitle(titre)

  return(p)
}
