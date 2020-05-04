#' Add together two numbers.
#'
#' This is truly a great and much-needed function
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @examples
#' aGreatFunction(1, 1)
#' aGreatFunction(10, 1)
myLinearRegression <- function(X, Y, sub){
  X2 <- X[sub,]
  if  (ncol(X)==1) X2 <- as.matrix(X2,ncol=1)
  Y2 <- Y[sub]
  res <- summary(lm(Y2~X2))
  ###This is not the plot I was asking for;   it's just intended to  give  you an  idea
  ###of what  to  do.
  plot(X2[,1],Y2)
  coef  <-  res$coef[,2]
  pvals <- res$coef[,4]
  return(list("coef"=coef,"pvals"=pvals))
}













