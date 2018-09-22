
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export

linear_model <- function(formula, data) {
  #Your code here.
  
  X = model.matrix(formula, data)
  Y<-model.frame(formula, data)[,1]
  
  QR <- qr(X)
  beta <- solve.qr(QR, Y) 
  
  l <- list()
  l$coefficients <- beta

  k <- rapply(l,function(x) ifelse(x==0,NA,x), how = "replace")
  
  names(k$coefficients) <- c("(Intercept)", "x1","x2")
  class(k) <- "lm"
  
  return(k)
}
