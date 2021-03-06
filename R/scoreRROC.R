#' @title Area Over the Curve for RROC Curves
#'
#' @description The area over the Regression Receiver Operating Characteristic.
#'
#' @param object An object of class ModelAudit.
#'
#' @return an object of class scoreAudit
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' scoreRROC(lm_au)
#'
#'
#' @seealso \code{\link{plotRROC}}
#'
#' @references Hernández-Orallo, José. 2013. ‘ROC Curves for Regression’. Pattern Recognition 46 (12): 3395–3411.
#'
#' @export


scoreRROC <- function(object){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")


  RROCF <- getRROCDF(object)
  x <- RROCF$RROCX
  y <- RROCF$RROCY

  aoc <- 0
  for (i in 2:(length(x) - 2)) {
    aoc <- aoc + 0.5 * (y[i+1] + y[i]) * (x[i+1] - x[i])
  }

  RROCResults <- list(
    name = "RROC",
    score = aoc
  )

  class(RROCResults) <- "scoreAudit"
  return(RROCResults)

}

# getRROCDF is in plotRROC.R file



