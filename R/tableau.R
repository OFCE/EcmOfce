#' Valeurs critiques d'Ericsson-MacKinnon
#'
#' Retourne les valeurs critiques aux seuils de 1%, 5% et 10% pour le test
#' de cointégration de Ericsson et MacKinnon (2000), selon les termes déterministes (d), le
#' nombre de variables dans x (k), la taille de l'échantillon (size) et le nombre de regresseurs non contraints (h),
#' en utilisant la formule de surface de réponse
#' q(Ti) = theta_inf + theta_1/(Ta)^1 + theta_2/(Ta)^2 + theta_3/(Ta)^3,
#' où Ta est la taille d'échantillon ajustée (size - h).
#' 
#' Référence : Ericsson, N. R., & MacKinnon, J. G. (2002). Distributions of
#' error correction tests for cointegration. Econometrics Journal, 5(2), 285-318.
#' (Tables 2, 3 et 4 pour les coefficients theta)
#'
#' @param d termes déterministes : \code{"nc"} (aucun), \code{"c"} (constante),
#'   \code{"ct"} (constante + trend). \code{"ctt"} n'est pas codé.
#' @param k nombre de variables dans x (entier entre 1 et 8)
#' @param size taille de l'échantillon
#' @param h nombre total de régresseurs (i.e. de coefficients non contraints)
#' @return vecteur numérique de longueur 3 : valeurs critiques à 1%, 5%, 10%
#' @export
ericsson_mackinnon_critical_val <- function(d, k, size, h) {
  if (isFALSE(d %in% c("nc", "c", "ct"))) {
    stop(sprintf("Argument 'd' doit être l'un de : %s", paste(c("nc", "c", "ct"), collapse = ", ")))
  }
  if (isFALSE(k >= 1 & k <= 8)) {
    stop("Les valeurs critiques ne sont codées que pour k compris entre 1 et 8")
  }
  
  # Fonction de création des matrices
  mat <- function(x) {
    x <- matrix(x, ncol = 4, byrow = TRUE)
    colnames(x) <- c("theta_inf", "theta_1", "theta_2", "theta_3")
    rownames(x) <- c("1pct", "5pct", "10pct")
    return(x)
  }
  
  # Coefficients theta - no deterministic terms - Table 2, Ericsson, MacKinnon (2000)
  ecm_nc <- list(
    k1 = mat(c(
      -2.5659, -2.19, -3.6, 26,
      -1.9408, -0.35, 0.6, -17,
      -1.6167, 0.23, -1.0, -6
    )),
    k2 = mat(c(
      -3.2106, -4.69, -10.5, 48,
      -2.5937, -1.53, -0.8, -24,
      -2.2643, -0.41, -1.5, -9
    )),
    k3 = mat(c(
      -3.6215, -6.14, -5.3, -67,
      -3.0048, -2.11, 2.1, -61,
      -2.6744, -0.57, 1.2, -44
    )),
    k4 = mat(c(
      -3.9433, -7.15, -3.1, -69,
      -3.3268, -2.04, -6.4, 19,
      -2.9942, -0.21, -5.1, 13
    )),
    k5 = mat(c(
      -4.2168, -7.66, -2.1, -87,
      -3.5978, -1.92, -3.6, -17,
      -3.2637, 0.25, -4.2, -15
    )),
    k6 = mat(c(
      -4.4585, -7.72, -7.2, -57,
      -3.8373, -1.38, -7.7, -6,
      -3.5022, 1.15, -11.1, 12
    )),
    k7 = mat(c(
      -4.6763, -7.78, -5.1, -73,
      -4.0535, -0.76, -10.0, -7,
      -3.7165, 2.04, -14.7, 15
    )),
    k8 = mat(c(
      -4.8772, -7.64, -2.4, -116,
      -4.2513, -0.03, -12.0, -19,
      -3.9135, 3.10, -20.3, 25
    ))
  )
  
  # Coefficients theta - constant term - Table 3, Ericsson, MacKinnon (2000)
  ecm_c <- list(
    k1 = mat(c(
      -3.4307, -6.52, -4.7, -10,
      -2.8617, -2.81, -3.2, 37,
      -2.5668, -1.56, 2.1, -29
    )),
    k2 = mat(c(
      -3.7948, -7.87, -3.6, -28,
      -3.2145, -3.21, -2.0, 17,
      -2.9083, -1.55, 1.9, -25
    )),
    k3 = mat(c(
      -4.0947, -8.59, -2.0, -65,
      -3.5057, -3.27, 1.1, -34,
      -3.1924, -1.23, 2.1, -39
    )),
    k4 = mat(c(
      -4.3555, -8.90, -6.7, -31,
      -3.7592, -2.92, -3.7, 5,
      -3.4412, -0.53, -4.5, 4
    )),
    k5 = mat(c(
      -4.5859, -9.14, -2.5, -78,
      -3.9856, -2.50, -1.7, -35,
      -3.6635, 0.21, -6.0, -8
    )),
    k6 = mat(c(
      -4.7970, -9.04, -5.6, -66,
      -4.1922, -1.73, -7.8, -9,
      -3.8670, 1.26, -12.7, 14
    )),
    k7 = mat(c(
      -4.9912, -8.85, -5.1, -72,
      -4.3831, -0.90, -12.2, 1,
      -4.0556, 2.39, -18.8, 27
    )),
    k8 = mat(c(
      -5.1723, -8.58, -2.0, -113,
      -4.5608, 0.02, -15.4, -2,
      -4.2310, 3.59, -25.6, 44
    ))
  )
  
  # Coefficients theta - constant term and linear trend - Table 4, Ericsson, MacKinnon (2000)
  ecm_ct <- list(
    k1 = mat(c(
      -3.9593, -8.99, -4.9, 39,
      -3.4108, -4.38, 4.5, -21,
      -3.1272, -2.57, 3.5, -7
    )),
    k2 = mat(c(
      -4.2488, -10.04, -4.1, -1,
      -3.6873, -4.56, 2.2, 1,
      -3.3927, -2.41, 3.4, -14
    )),
    k3 = mat(c(
      -4.4981, -10.69, 0.6, -58,
      -3.9263, -4.47, 5.2, -38,
      -3.6249, -1.86, 1.1, -10
    )),
    k4 = mat(c(
      -4.7214, -10.94, 1.6, -77,
      -4.1421, -3.99, 2.8, -35,
      -3.8342, -1.16, 0.4, -23
    )),
    k5 = mat(c(
      -4.9255, -10.86, 1.2, -94,
      -4.3392, -3.37, 1.6, -47,
      -4.0271, -0.17, -4.4, -14
    )),
    k6 = mat(c(
      -5.1137, -10.72, 1.4, -96,
      -4.5227, -2.52, -2.8, -32,
      -4.2067, 0.94, -9.9, 0
    )),
    k7 = mat(c(
      -5.2923, -10.11, -4.0, -75,
      -4.6952, -1.43, -10.6, -5,
      -4.3751, 2.18, -16.9, 18
    )),
    k8 = mat(c(
      -5.4565, -9.77, -1.5, -106,
      -4.8569, -0.43, -14.4, -3,
      -4.5344, 3.52, -24.9, 40
    ))
  )
  
  tables <- list(nc = ecm_nc, c = ecm_c, ct = ecm_ct)
  vec_theta <- tables[[d]][[paste0("k", k)]]
  
  # critical value
  Ta <- size - h
  critical_val <- vec_theta[, "theta_inf"] +
    vec_theta[, "theta_1"] / Ta +
    vec_theta[, "theta_2"] / Ta^2 +
    vec_theta[, "theta_3"] / Ta^3
  
  return(critical_val)
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

  endog <- all.vars(stats::formula(estim))[1]
  check_presence_var_ecart_lt <- any(grepl("ecart", names(estim$coefficients)) & grepl("lt", names(estim$coefficients)))
  var_lt <- grep("^(?!.*delta).*lag.*$", names(estim$coefficients), value = TRUE, perl = TRUE) # var de LT avec lag() et pas delta
  if (isTRUE(check_presence_var_ecart_lt)){
    var_force_rappel <- names(estim$coefficients)[grep("ecart", names(estim$coefficients))]
  }else{
    var_force_rappel <- var_lt[grepl(endog, var_lt)]
  }
  var_lt <- setdiff(var_lt, var_force_rappel)

  # Vérifie s'il y a une constante et trend => d
  has_intercept <- attr(stats::terms(estim), "intercept") == 1
  has_trend <- any(grepl("temps|trend", names(estim$coefficients), ignore.case = TRUE))
  d <- dplyr::case_when(
    has_intercept && has_trend ~ "ct",
    has_intercept              ~ "c",
    TRUE                       ~ "nc"
  )
  
  # Nombre de variables dans x => k
  k <- length(all.vars(stats::as.formula(paste("~", paste(c(var_force_rappel, var_lt), collapse = "+")))))
  k <- ifelse(check_presence_var_ecart_lt, k + 1, k) # on imagine que la variable ecart=y-z donc je rajoute 1 variable
  # Nombre de régresseurs (non contraints) => h (j'ai inclus les dummy)
  h <- length(estim$coefficients)
  
  # Les valeurs critiques correspondantes selon Ericsson-Mackinnon aux seuils de 1%,5%,10%
  critical_val <- ericsson_mackinnon_critical_val(d = d, k = k, size = stats::nobs(estim), h = h)

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

  return(stats::setNames(coef_info$value, coef_info$term))
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
make_table_ecm <- function(table_resultats, data, estim, nom_col, affiche_dum = TRUE, divise_fr = TRUE) {
  vars  <- names(estim$coefficients)
  endog <- all.vars(stats::formula(estim))[1]
  
  # Vérifie que la variable dépendante est bien un delta(log(.))
  is_delta_log <- grepl("delta\\(1, log\\(", stats::formula(estim)[2])
  if (!is_delta_log) {
    stop("La variable dépendante n'est pas un delta(log(.))")
  }
  is_delta_delta_log <- grepl("delta\\(1, delta\\(1, log\\(", stats::formula(estim)[2]) # estim Phillips notamment
  if (is_delta_delta_log) {
    stop("Variable endogène en delta(delta(log(.))) : cette fonction n'est pas adaptée, prendre dans le code fonctions_prix_salaires.R")
  }
  
  # Coefs imposés avec offset() ? si oui, on les ajoute aux variables pour les afficher dans le tableau
  offset_var <- grep("^offset\\(", colnames(estim$model), value = TRUE)
  if (length(offset_var) > 0) {
    noms_offset <- sub("^offset\\((.*)\\)$", "\\1", offset_var)
    vars <- c(vars, noms_offset)
  }
  
  # Variables de long-terme, court-terme et force de rappel, puis extraction des coeff
  check_presence_var_ecart_lt <- any(grepl("ecart", names(estim$coefficients)) & grepl("lt", names(estim$coefficients)))
  var_lt <- grep("^(?!.*delta).*lag.*$", names(estim$coefficients), value = TRUE, perl = TRUE)
  var_ct <- setdiff(vars, var_lt)
  if (check_presence_var_ecart_lt) {
    var_force_rappel <- vars[grep("ecart", vars)]
    message(glue::glue("Le coefficient de la variable {var_force_rappel} est la force de rappel"))
  } else {
    var_force_rappel <- var_lt[grepl(endog, var_lt)]
  }
  var_lt <- setdiff(var_lt, var_force_rappel)
  
  ecm_coefs <- coeff_tableau(estim, divise_fr = divise_fr)

  # Coefs imposés à 1 via offset() : on rajoute "1 <br>(c)"
  if (length(offset_var) > 0) {
    coef_offset <- stats::setNames(rep("1 <br>(c)", length(noms_offset)), noms_offset)
    ecm_coefs <- c(coef_offset, ecm_coefs)
  }
  
  # Coefs imposés dans la relation de long-terme, ex I(x - z)
  if (grepl("^I\\(.*\\)$", var_force_rappel)) {
    expr  <- paste0("+ ", sub("^I\\((.*)\\)$", "\\1", var_force_rappel))
    ops   <- regmatches(expr, gregexpr("[+-]", expr))[[1]]
    terms <- trimws(unlist(strsplit(sub("^[+-]", "", expr), "\\s*[+-]\\s*")))
    keep  <- !grepl(endog, terms)
    terms <- terms[keep]
    ops   <- ops[keep]
    coef_impose <- stats::setNames(ifelse(ops == "-", "1 <br>(c)", "-1 <br>(c)"), terms)
    ecm_coefs   <- c(coef_impose, ecm_coefs)
    var_lt      <- c(var_lt, terms)
  }
  
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
  table_resultats <- dplyr::select(table_resultats, -dum)
  
  return(table_resultats)
}
