#' Unit tests for models
#'
#' Currently three tests are performed
#'  - for outliers in residuals
#'  - for autocorrelation in target variable or in residuals
#'  - for trend in residuals as a function of target variable (detection of bias)
#'
#' @param x An object of class `modelAudit`
#' @param ... Other object of class `modelAudit`
#' @param tests A vector of test functions' names
#' @param new_tests a path to file with test.
#'
#' @return list with statistics for particular checks
#' @import cli
#'
#' @export
test_models <- function(x, ...,
                 tests = c("test_response_autocorrelation", "test_residuals_autocorrelation"),
                 new_tests = NULL){

  aul <- c(list(x), list(...))

  models <- sapply(aul, function(x)x$label)

  message(paste("Auditing models:", paste(models, collapse =", ")))
  Sys.sleep(0.05)
  utils::flush.console()

  # width of column equals 9
  models_short <- substr(models, 1, 7)
  spaces <- sapply(models_short, function(x) paste(rep(" ", 7 - nchar(x)), collapse = ""))
  models_short <- paste0(models_short, spaces)

  models_header <- crayon::bold(" ", paste(models_short, collapse = " | "), "|")
  values_header <- crayon::bold(" ", paste(rep(paste0("value ", cli::symbol$tick), length(models)), collapse = " | "), "| test")
  cat_line(models_header)
  cat_line(values_header)

  for(test in tests){
    result <- switch (test,
      "test_response_autocorrelation" = sapply(aul, function(x) test_response_autocorrelation(x)),
      "test_residuals_autocorrelation" = sapply(aul, function(x) test_residuals_autocorrelation(x))
    )
    test_values <- paste(paste(" ", paste(sprintf( "%+1.2f   ", result), collapse = " | ")), test, sep = " | ")
    cat_line(test_values)
  }
}


test_response_autocorrelation <- function(x, method = "pearson"){
  y <- x$y
  ctest_y <- cor.test(y[-1], y[-length(y)], method = method)
  return(ctest_y$estimate)
}

test_residuals_autocorrelation <- function(x, method = "pearson"){
  residuals <- x$residuals
  ctest_r   <- cor.test(residuals[-1], residuals[-length(residuals)], method = method)
  return(ctest_r$estimate)
}
