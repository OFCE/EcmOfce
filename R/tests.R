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
