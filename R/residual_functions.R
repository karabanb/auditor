getResidualFunction <- function(residual.function){
  if (!is.null(residual.function)) {
    res <- function(model, y=NULL, data=NULL, predict.function=NULL){residual.function(model, data, y)}
  } else {
    res <- getResiduals
  }
  return(res)
}

getResiduals <- function(model, data=NULL, y = NULL, predict.function=NULL){
    if(is.factor(y)) y <- as.numeric(y) - 1
    res <-  y - predict.function(model, data)

  return(res)
}
