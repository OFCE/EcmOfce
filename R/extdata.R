#' Copy example files to a directory
#'
#' Copies the example scripts and dataset bundled in the package
#' (\code{inst/extdata}) to a local directory, so you can open and run them
#' directly. Files included: \code{data.rds}, \code{exemple_tab.qmd},
#' \code{test.R}.
#'
#' @param dest destination directory (default: current working directory)
#' @param overwrite logical; overwrite existing files? (default: \code{FALSE})
#' @return invisibly, a named logical vector of copy successes (one per file)
#' @export
copy_examples <- function(dest = getwd(), overwrite = FALSE) {
  src_dir <- system.file("extdata", package = "EcmOfce")
  if (!nzchar(src_dir)) {
    stop("EcmOfce inst/extdata directory not found. Is the package installed?")
  }

  files <- list.files(src_dir, full.names = FALSE)
  if (length(files) == 0) {
    message("No example files found in inst/extdata.")
    return(invisible(logical(0)))
  }

  if (!dir.exists(dest)) {
    dir.create(dest, recursive = TRUE)
    message("Created directory: ", dest)
  }

  results <- vapply(files, function(f) {
    from <- file.path(src_dir, f)
    to   <- file.path(dest, f)
    if (file.exists(to) && !overwrite) {
      message("Skipped (already exists): ", f, "  [use overwrite = TRUE to replace]")
      return(FALSE)
    }
    ok <- file.copy(from, to, overwrite = overwrite)
    if (ok) message("Copied: ", f, " -> ", dest) else message("Failed:  ", f)
    ok
  }, logical(1))

  invisible(results)
}
