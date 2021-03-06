#' @title Plot Residuals vs Observed, Fitted or Variable Values in D3 with r2d3 Package.
#'
#' Function \code{plotD3Residual} plots resudial values vs observed, fitted or variable values in the model.
#' It uses output from \code{modelAudit} or \code{modelResiduals} function.
#'
#' If the picture is not displayed in the viewer, please update your RStudio.
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Name of model variable to order residuals. If value is NULL data order is taken or variable from modelResiduals object. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param points Logical, indicates whenever observations should be added as points. By defaul it's TRUE.
#' @param smooth Logical, indicates whenever smoothed lines should be added. By default it's FALSE.
#' @param std_residuals Logical, indicates whenever standardized residuals should be used. By default it's FALSE.
#' @param point_count Number of points to be plotted per model. Points will be chosen randomly. By default plot all of them.
#' @param single_plot Logical, indicates whenever single or facets should be plotted. By default it's TRUE.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#' @param background Logical, available only if single_plot = FALSE. Indicates whenever backgroud plots should be plotted. By default it's FALSE.
#'
#' @return an `r2d3` object.
#'
#' @examples
#' library(auditor)
#' library(car)
#'
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, label = "lm")
#' plotD3Residual(lm_au, variable = "income")
#'
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, label = "rf")
#' rf_mr <- modelResiduals(rf_au, "income")
#' plotD3Residual(lm_au, rf_mr, variable = "income", smooth = TRUE)
#' plotD3Residual(lm_au, rf_mr, variable = "income", smooth = TRUE, single_plot = FALSE)
#'
#' @seealso \code{\link{plotResidual}}
#'
#' @export
#' @rdname plotD3Residual

plotD3Residual <- function(object, ..., variable = NULL, points = TRUE, smooth = FALSE, std_residuals = FALSE,
                            point_count = NULL, single_plot = TRUE, scale_plot = FALSE, background = FALSE){

  if (points == FALSE & smooth == FALSE) stop("Plot points or smooth.")

  n <- length(list(...)) + 1

  aul <- list(object, ...)

  # chose y
  if (std_residuals == TRUE) {
    y <- "std.res"
    yTitle <- "standardized residuals"
    chartTitle <- "Standardized residuals vs"
  } else {
    y <- "res"
    yTitle <- "residuals"
    chartTitle <- "Residuals vs"
  }

  # make every input modelResiduals, check `variable`
  mrl <- list()
  varl <- c()

  for (i in 1:n) {
    object <- aul[[i]]

    if (!any(class(object) %in%  c("modelAudit","modelResiduals"))) stop("The function requires an object created with audit() or modelResiduals().")
    if (!("modelResiduals" %in% class(object))) {
      mr <- modelResiduals(object, variable)
    } else {
      mr <- object
    }

    varl <- c(varl, as.character(mr$variable[1]))

    df <- mr[, c(y, "val", "label")]
    class(df) <- "data.frame"
    colnames(df) <- c("res", "val", "label")
    mrl[[i]] <- df
  }

  if (length(unique(varl)) > 1) {
    stop("Objects have more than one variable name.")
  } else {
    if (is.na(unique(varl))) {
      chartTitle <- "Residuals"
      variable <- "observations"
    } else {
      variable <- varl[1]
      chartTitle <- paste(chartTitle, variable)
    }
  }

  modelNames <- unlist(lapply(mrl, function(x) unique(x$label)))
  pointMax <- pointMin <- smoothMax <- smoothMin <- NULL
  pointData <- smoothData <- NA

  # prepare points data
  if (points == TRUE) {

    # find instance count and adjust point_count
    m <- dim(mrl[[1]])[1]
    if (is.null(point_count) || point_count > m) {
      pointData <- mrl
    } else {
      pointData <- lapply(mrl, function(mr) {
        mr <- mr[sample(m,point_count),]
        mr
      })
    }

    names(pointData) <- modelNames
    pointMax <- max(sapply(mrl, function(x) max(x$res)))
    pointMin <- min(sapply(mrl, function(x) min(x$res)))
  }

  # prepare smooth data
  if (smooth == TRUE) {

    smoothData <- lapply(mrl, function(mr) {
      model <- mgcv::gam(res ~ s(val, bs = "cs"), data = mr)
      vec <- data.frame(val = seq(min(mr$val), max(mr$val), length.out = 100))
      p <- predict(model, vec)
      df <- data.frame(val = vec$val, smooth = as.numeric(p))
      dim(df$val) <- NULL
      df
    })

    names(smoothData) <- modelNames
    smoothMax <- max(sapply(smoothData, function(x) max(x$smooth)))
    smoothMin <- min(sapply(smoothData, function(x) min(x$smooth)))
  }

  # find x and y scale
  xmax <- max(mrl[[1]]$val)
  xmin <- min(mrl[[1]]$val)
  ymax <- max(pointMax, smoothMax)
  ymin <- min(pointMin, smoothMin)

  ticksMargin <- abs(ymin-ymax)*0.15;

  temp <- jsonlite::toJSON(list(pointData, smoothData))

  options <- list(xmax = xmax, xmin = xmin,
                  ymax = ymax + ticksMargin, ymin = ymin - ticksMargin,
                  variable = variable, n = n,
                  points = points, smooth = smooth,
                  scalePlot = scale_plot, yTitle = yTitle, chartTitle = chartTitle)

  if (single_plot == TRUE) {

    r2d3::r2d3(data = temp, script = system.file("d3js/plotResidualsSingle.js", package = "auditor"),
           dependencies = system.file("d3js/colorsDrWhy.js", package = "auditor"),
           css = system.file("d3js/themeDrWhy.css", package = "auditor"),
           d3_version = 4,
           options = options)

  } else {
    if (n==1) stop("Use single_plot instead.")
    options['background'] <- background

    r2d3::r2d3(data = temp, system.file("d3js/plotResidualsMany.js", package = "auditor"),
           dependencies = system.file("d3js/colorsDrWhy.js", package = "auditor"),
           css = system.file("d3js/themeDrWhy.css", package = "auditor"),
           d3_version = 4,
           options = options)
  }
}
