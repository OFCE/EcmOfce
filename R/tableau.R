#' Valeurs critiques d'Ericsson-MacKinnon
#'
#' Retourne les valeurs critiques aux seuils de 1%, 5% et 10% pour le test
#' de cointégration d'Ericsson-MacKinnon selon les termes déterministes et le
#' nombre de régresseurs.
#'
#' @param d termes déterministes : \code{"nc"} (aucun), \code{"c"} (constante),
#'   \code{"ct"} (constante + trend), \code{"ctt"} (constante + deux trends)
#' @param k nombre de régresseurs (entier entre 1 et 11)
#' @return vecteur numérique de longueur 3 : valeurs critiques à 1%, 5%, 10%
#' @export
ericsson_mackinnon_critical_val <- function(d, k) {
  d_autorise <- c("nc", "c", "ct", "ctt")
  if (!(d %in% d_autorise)) {
    stop(sprintf("Argument 'd' doit etre l'un de : %s", paste(d_autorise, collapse = ", ")))
  }

  table_cv <- list(
    nc = rbind(
      c(-2.5659, -1.9408, -1.6167),
      c(-3.2106, -2.5937, -2.2643),
      c(-3.6215, -3.0048, -2.6744),
      c(-3.9433, -3.3268, -2.9942),
      c(-4.2168, -3.5978, -3.2637),
      c(-4.4585, -3.8373, -3.5022),
      c(-4.6763, -4.0535, -3.7165),
      c(-4.8772, -4.2513, -3.9135),
      c(-5.0634, -4.4363, -4.2693),
      c(-5.2381, -4.6093, -4.2693),
      c(-5.4039, -4.7734, -4.4324)
    ),
    c = rbind(
      c(-3.4307, -2.8617, -2.5668),
      c(-3.7948, -3.2145, -2.9083),
      c(-4.0947, -3.5057, -3.1924),
      c(-4.3555, -3.7592, -3.4412),
      c(-4.5859, -3.9856, -3.6635),
      c(-4.7970, -4.1922, -3.8670),
      c(-4.9912, -4.3831, -4.0556),
      c(-5.1723, -4.5608, -4.2310),
      c(-5.3437, -4.7287, -4.3975),
      c(-5.5048, -4.8876, -4.5543),
      c(-5.6588, -5.0394, -4.7055)
    ),
    ct = rbind(
      c(-3.9593, -3.4108, -3.1272),
      c(-4.2488, -3.6873, -3.3927),
      c(-4.4981, -3.9263, -3.6249),
      c(-4.7214, -4.1421, -3.8342),
      c(-4.9255, -4.3392, -4.0271),
      c(-5.1137, -4.5227, -4.2067),
      c(-5.2923, -4.6952, -4.3751),
      c(-5.4565, -4.8569, -4.5344),
      c(-5.6149, -5.0108, -4.6864),
      c(-5.7657, -5.1582, -4.8311),
      c(-5.9099, -5.2992, -4.9707)
    ),
    ctt = rbind(
      c(-4.3714, -3.8324, -3.5534),
      c(-4.6190, -4.0683, -3.7800),
      c(-4.8399, -4.2790, -3.9833),
      c(-5.0396, -4.4716, -4.1701),
      c(-5.2256, -4.6498, -4.3438),
      c(-5.3998, -4.8177, -4.5073),
      c(-5.5652, -4.9774, -4.6629),
      c(-5.7181, -5.1265, -4.8098),
      c(-5.8656, -5.2703, -4.9510),
      c(-6.0083, -5.4083, -5.0863),
      c(-6.1449, -5.5415, -5.2176)
    )
  )

  if (k < 1 || k > 11) {
    stop("k doit etre compris entre 1 et 11.")
  }
  table_cv[[d]][k, ]
}


#' Extraction et formatage des coefficients d'un MCE
#'
#' Combine coefficient, étoiles de significativité et t-statistique sous la forme
#' \code{"0.123*** <br>(2.45)"}. Pour les variables de long terme, le coefficient
#' est divisé par la force de rappel (élasticité de long terme). La force de rappel
#' reçoit des étoiles selon les valeurs critiques d'Ericsson-MacKinnon.
#'
#' @param estim objet \code{lm} issu de l'estimation du MCE
#' @param divise_fr logique ; si \code{TRUE} (défaut), les coefficients de long terme
#'   sont divisés par la force de rappel
#' @return vecteur nommé de chaînes de caractères (HTML), un élément par variable
#' @export
coeff_tableau <- function(estim, divise_fr = TRUE) {
  coef_info <- broom::tidy(estim)
  coef_info <- dplyr::mutate(
    coef_info,
    stars = dplyr::case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE ~ ""
    ),
    value = glue::glue("{round(estimate, 3)}{stars} <br>({round(statistic, 2)})")
  )

  endog          <- all.vars(formula(estim))[1]
  var_ecart_lt   <- names(estim$coefficients)[grep("ecart", names(estim$coefficients))]
  var_lt         <- grep("^(?!.*delta).*lag.*$", names(estim$coefficients), value = TRUE, perl = TRUE)
  if (length(var_ecart_lt) != 0) {
    var_force_rappel <- var_ecart_lt
  } else {
    var_force_rappel <- var_lt[grepl(endog, var_lt)]
  }
  var_lt <- setdiff(var_lt, var_force_rappel)

  vars               <- unique(sub(".*\\.", "", all.vars(formula(estim))))
  vars_sans_dummies  <- vars[!grepl("^i.*q.*", vars)]
  k                  <- ifelse(length(var_ecart_lt) == 1, length(vars_sans_dummies) - 1, length(vars_sans_dummies))

  has_intercept <- attr(terms(estim), "intercept") == 1
  has_trend     <- any(grepl("temps|trend", vars, ignore.case = TRUE))
  d <- dplyr::case_when(
    has_intercept && has_trend ~ "ct",
    has_intercept              ~ "c",
    TRUE                       ~ "nc"
  )

  critical_val <- ericsson_mackinnon_critical_val(d = d, k = k)

  coef_info <- dplyr::mutate(
    coef_info,
    stars = dplyr::case_when(
      term == var_force_rappel & statistic < critical_val[1] ~ "***",
      term == var_force_rappel & statistic < critical_val[2] ~ "**",
      term == var_force_rappel & statistic < critical_val[3] ~ "*",
      term == var_force_rappel & statistic >= critical_val[3] ~ "",
      TRUE ~ stars
    ),
    estimate = dplyr::case_when(
      term %in% var_lt & divise_fr ~ -estimate / estimate[term == var_force_rappel],
      TRUE ~ estimate
    ),
    value = glue::glue("{round(estimate, 3)}{stars} <br>({round(statistic, 2)})")
  )

  stats::setNames(coef_info$value, coef_info$term)
}


#' Tableau de résultats d'un MCE estimé en une étape
#'
#' Ajoute les coefficients, la période d'estimation, le R² ajusté et les tests
#' diagnostics dans un tableau cumulatif, permettant de comparer plusieurs
#' spécifications côte à côte.
#'
#' @param table_resultats tibble existant ou \code{NULL} pour démarrer un nouveau tableau
#' @param data data.frame contenant au moins une colonne \code{date}
#' @param estim objet \code{lm} issu de l'estimation du MCE
#' @param nom_col nom (chaîne) de la colonne à ajouter
#' @param affiche_dum logique ; si \code{TRUE} (défaut), les indicatrices sont incluses
#' @param divise_fr logique ; si \code{TRUE} (défaut), les coefficients de long terme
#'   sont divisés par la force de rappel (voir \code{\link{coeff_tableau}})
#' @return tibble avec les colonnes \code{Groupe}, \code{Variables} et \code{nom_col}
#' @export
fct_tableau_ecm <- function(table_resultats, data, estim, nom_col, affiche_dum = TRUE, divise_fr = TRUE) {
  endog        <- all.vars(formula(estim))[1]
  var_ecart_lt <- names(estim$coefficients)[grep("ecart", names(estim$coefficients))]
  var_lt       <- grep("^(?!.*delta).*lag.*$", names(estim$coefficients), value = TRUE, perl = TRUE)
  var_ct       <- setdiff(names(estim$coefficients), c(var_ecart_lt, var_lt))
  if (length(var_ecart_lt) != 0) {
    var_force_rappel <- var_ecart_lt
  } else {
    var_force_rappel <- var_lt[grepl(endog, var_lt)]
  }
  var_lt <- setdiff(var_lt, var_force_rappel)

  ecm_coefs <- coeff_tableau(estim, divise_fr = divise_fr)

  lignes_utilisees  <- as.numeric(rownames(estim$model))
  periodes_utilisees <- data$date[lignes_utilisees]
  periode <- paste0(
    lubridate::year(min(periodes_utilisees)), "T", lubridate::quarter(min(periodes_utilisees)),
    "-",
    lubridate::year(max(periodes_utilisees)), "T", lubridate::quarter(max(periodes_utilisees))
  )

  tests_ecm <- make_tests(estim, banque = data)$tests
  r2_ecm    <- summary(estim)$adj.r.squared

  new_col <- tibble::tibble(
    Variables = c(
      names(ecm_coefs),
      "Periode d'estimation", "R2-adj", "Jarque-Bera", "LM1", "LM4", "ARCH1"
    ),
    !!nom_col := c(
      ecm_coefs,
      periode,
      round(r2_ecm, 2),
      glue::glue("{round(tests_ecm[, 1], 2)} <br>[p={round(tests_ecm[, 2], 2)}]")
    )
  )

  new_col <- dplyr::mutate(
    new_col,
    Variables = dplyr::case_when(
      Variables == var_force_rappel ~ "Force de rappel",
      Variables == "(Intercept)"    ~ "Constante",
      TRUE                          ~ Variables
    ),
    Groupe = dplyr::case_when(
      Variables %in% c("Force de rappel", "Constante")                                           ~ "Force de rappel et constante",
      Variables %in% var_lt                                                                        ~ "Coefficients de long-terme",
      grepl("^i.*q.*", Variables)                                                                  ~ "Indicatrices",
      Variables %in% var_ct                                                                         ~ "Coefficients de court-terme",
      Variables %in% c("Periode d'estimation", "R2-adj", "Jarque-Bera", "LM1", "LM4", "ARCH1")  ~ "Statistiques"
    )
  )

  if (!affiche_dum) {
    new_col <- dplyr::filter(new_col, Groupe != "Indicatrices")
  }

  if (is.null(table_resultats)) {
    table_resultats <- new_col
  } else {
    table_resultats <- dplyr::full_join(table_resultats, new_col, by = c("Groupe", "Variables"))
  }

  ordre <- c(
    "Force de rappel et constante", "Coefficients de long-terme",
    "Coefficients de court-terme", "Statistiques", "Indicatrices"
  )
  table_resultats <- dplyr::mutate(
    table_resultats,
    dplyr::across(dplyr::everything(), ~ tidyr::replace_na(as.character(.), "\\-")),
    Groupe = factor(Groupe, levels = ordre),
    dum    = ifelse(grepl("^i.*q.*", Variables), Variables, NA)
  )
  table_resultats <- dplyr::arrange(table_resultats, Groupe, dum)
  dplyr::select(table_resultats, -dum)
}
