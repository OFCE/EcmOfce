#' Tests diagnostics sur les résidus d'une estimation
#'
#' Calcule les tests de normalité (Jarque-Bera), d'autocorrélation (Breusch-Godfrey)
#' et d'hétéroscédasticité (ARCH) sur les résidus d'un objet \code{lm}.
#'
#' @param estim objet \code{lm} issu de l'estimation
#' @param banque data.frame contenant les données de l'estimation
#' @param ordre.lm ordre maximal pour le test de Breusch-Godfrey (défaut : 4)
#' @param ordre.arch ordre maximal pour le test ARCH (défaut : 4)
#' @return liste avec trois éléments : \code{resume} (summary de l'estimation),
#'   \code{R2adj} (R² ajusté), \code{tests} (data.frame des statistiques et p-values)
#' @export
make_tests <- function(estim, banque, ordre.lm = 4, ordre.arch = 4) {
  residus <- residuals(estim)
  resume <- summary(estim)
  SST <- sum(estim[["model"]][1]^2)
  SSR <- sum(resume$residuals^2)
  R2 <- 1 - SSR / SST
  R2adj <- c("R2-adj : ", 1 - ((1 - R2) * (resume$df[1] + resume$df[2]) / (resume$df[2])))

  test.normal <- tseries::jarque.bera.test(residus)

  bg_dap <- function(x) {
    lmtest::bgtest(formula(estim), order = x, type = c("Chisq", "F"), data = banque)
  }
  bg.liste <- lapply(seq_len(ordre.lm), bg_dap)

  arch_dap <- function(x) {
    FinTS::ArchTest(residus, lags = x, demean = FALSE)
  }
  arch.liste <- lapply(seq_len(ordre.arch), arch_dap)

  tableau_test <- list(
    statistique = c(
      test.normal[["statistic"]],
      bg.liste[[1]][["statistic"]],
      bg.liste[[4]][["statistic"]],
      arch.liste[[4]][["statistic"]]
    ),
    p.value = c(
      test.normal[["p.value"]],
      bg.liste[[1]][["p.value"]],
      bg.liste[[4]][["p.value"]],
      arch.liste[[4]][["p.value"]]
    )
  )
  tableau_test <- as.data.frame(tableau_test)
  rownames(tableau_test) <- c("jarque-bera", "LM1", "LM4", "ARCH1")

  list(resume = resume, R2adj = R2adj, tests = tableau_test)
}


#' Test de stationnarité ADF + KPSS
#'
#' Combine les tests ADF (Augmented Dickey-Fuller) et KPSS pour diagnostiquer
#' l'ordre d'intégration d'une série, et imprime une conclusion automatique.
#'
#' @param y vecteur numérique (la série à tester)
#' @param ordre ordre d'intégration à tester : 0 (série brute), 1 (première
#'   différence) ou 2 (deuxième différence)
#' @param alpha seuil de significativité : 0.01, 0.05 (défaut) ou 0.10
#' @return chaîne de caractères avec la conclusion (retournée invisiblement
#'   après affichage)
#' @export
stationarity_check <- function(y, ordre = 0, alpha = 0.05) {
  if (!(ordre %in% c(0, 1, 2))) {
    stop("L'ordre d'integration doit etre 0, 1 ou 2.")
  }
  if (ordre == 1) y <- diff(y)
  if (ordre == 2) y <- diff(y, 2)

  seuil <- switch(as.character(alpha),
    "0.01"  = "1pct",
    "0.05"  = "5pct",
    "0.1"   = "10pct",
    stop("alpha doit etre 0.01, 0.05 ou 0.10.")
  )

  cat("=====================================\n")
  cat("Analyse stationnarite (ADF + KPSS)\n")
  cat(sprintf("Ordre d'integration I(%d)\n", ordre))
  cat("=====================================\n\n")

  adf_drift <- urca::ur.df(stats::na.omit(y), type = "drift", selectlags = "AIC")
  adf_trend <- urca::ur.df(stats::na.omit(y), type = "trend", selectlags = "AIC")

  stat_adf_drift  <- adf_drift@teststat[1]
  crit_adf_drift  <- adf_drift@cval[1, seuil]
  stat_adf_trend  <- adf_trend@teststat[1]
  crit_adf_trend  <- adf_trend@cval[1, seuil]
  adf_drift_reject <- stat_adf_drift < crit_adf_drift
  adf_trend_reject <- stat_adf_trend < crit_adf_trend

  kpss_level <- tseries::kpss.test(stats::na.omit(y), null = "Level")
  kpss_trend <- tseries::kpss.test(stats::na.omit(y), null = "Trend")
  kpss_level_reject <- kpss_level$p.value < alpha
  kpss_trend_reject <- kpss_trend$p.value < alpha

  cat("ADF drift : stat =", round(stat_adf_drift, 3),
      "| crit", seuil, "=", crit_adf_drift,
      "| lags =", adf_drift@lags,
      "| rejet H0 (racine unitaire) =", adf_drift_reject, "\n")
  cat("ADF trend : stat =", round(stat_adf_trend, 3),
      "| crit", seuil, "=", crit_adf_trend,
      "| lags =", adf_trend@lags,
      "| rejet H0 (racine unitaire) =", adf_trend_reject, "\n\n")
  cat("KPSS level : stat =", round(kpss_level$statistic, 3),
      "| p-value =", round(kpss_level$p.value, 3),
      "| lags =", kpss_level$parameter,
      "| rejet H0 (absence racine unitaire) =", kpss_level_reject, "\n")
  cat("KPSS trend : stat =", round(kpss_trend$statistic, 3),
      "| p-value =", round(kpss_trend$p.value, 3),
      "| lags =", kpss_trend$parameter,
      "| rejet H0 (absence racine unitaire) =", kpss_trend_reject, "\n\n")

  conclusion <- if (adf_drift_reject && !kpss_level_reject) {
    paste0(">>> Conclusion : serie I(", ordre, ") autour d'une constante")
  } else if (adf_trend_reject && !kpss_trend_reject) {
    paste0(">>> Conclusion : serie I(", ordre, ") autour d'une tendance deterministe")
  } else if (!adf_drift_reject && !kpss_level_reject) {
    paste0(">>> Conclusion : I(", ordre, ") d'apres KPSS niveau mais pas d'apres ADF")
  } else if (!adf_trend_reject && !kpss_trend_reject) {
    paste0(">>> Conclusion : I(", ordre, ") d'apres KPSS trend mais pas d'apres ADF")
  } else if (adf_drift_reject && kpss_level_reject) {
    paste0(">>> Conclusion : I(", ordre, ") d'apres ADF mais pas d'apres KPSS niveau")
  } else if (adf_trend_reject && kpss_trend_reject) {
    paste0(">>> Conclusion : I(", ordre, ") d'apres ADF mais pas d'apres KPSS trend")
  } else {
    paste0(">>> Conclusion : Pas I(", ordre, ")")
  }

  cat(conclusion, "\n")
  invisible(conclusion)
}
