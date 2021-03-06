#' @title Autocorrelation Plot
#'
#' @description Plot of i-th residual vs i+1-th residual.
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Only for modelAudit object. Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param score Logical, if TRUE values of \link{scoreDW} and \link{scoreRuns} will be added to plot.
#' @param lines Logical, if TRUE smooth line will be added.
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotAutocorrelation(lm_au)
#'
#' @import ggplot2
#'
#' @export
plotAutocorrelation <- function(object, ..., variable = NULL, score = FALSE, lines = FALSE) {
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if("modelResiduals" %in% class(object)) variable <- object$variable[1]
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object, variable)

  x <- y <- NULL

  df <- object
  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if ("modelAudit" %in% class(resp)) df <- rbind( df, modelResiduals(resp, variable) )
      if ("modelResiduals" %in% class(resp)) df <- rbind(df, resp)
    }
  }

  resultDF <- data.frame(x = numeric(), y = numeric(), label = character())
  for (label in unique(df$label)){
    orderedResiduals <- df[which(df$label == label), "res"]
    n <- length(orderedResiduals)
    resultDF <- rbind(resultDF, data.frame(x = orderedResiduals[-n], y = orderedResiduals[-1], label = label))
  }

  maybe_lines <- if (lines == TRUE) resultDF else resultDF[0, ]

  p <- ggplot(resultDF, aes(x, y, color = label)) +
    geom_point(data = resultDF, alpha = ifelse(lines == TRUE, 0.95, 1), stroke = 0) +
    geom_line(data = maybe_lines,
              aes(x, y, colour = factor(label, levels = rev(levels(maybe_lines$label)))),
              stat = "smooth",
              method = "loess",
              se = FALSE,
              size = 1,
              show.legend = TRUE) +
    scale_color_manual(values = c(theme_drwhy_colors(length(unique(df$label))))) +
    xlab("residual i") +
    ylab("residual i+1") +
    ggtitle("Autocorrelation plot") +
    theme_drwhy()

  if (score == TRUE) {
    score1 <- scoreDW(object, variable)
    score2 <- scoreRuns(object, variable)
    caption <- paste("Durbin-Watson Score:", round(score1$score, 2), "\nRuns Score:", round(score2$score, 2))
    p <- p + ggplot2::annotate("text",
                               x = min(resultDF$x),
                               y = min(resultDF$y),
                               label = caption,
                               hjust = 0,
                               vjust = 0,
                               size = 3.5, colour = "#ae2c87")
  }

  return(p)
}
