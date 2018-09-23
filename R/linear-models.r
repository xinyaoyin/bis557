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

 # Construct Design Matrix X
   X<-model.matrix(formula, data)

 # Get Response Variable Y which is the first column of every dataset 
   Y<-model.frame(formula, data)[,1]

 # Use QR decomposition to decompose the matrix X 
   QR <- qr(X)
  
 # Calculate the beta  
   beta <- solve.qr(QR, Y) 
   beta[which(beta==0)] <- NA

 # Put beta into a list and set the class to lm (Discussed with Patty Zhang)
  
   l = list(coefficients = beta, residals = X %*% beta, fitted.values = Y - X %*% beta, rank = ncol(X), df.residual = nrow(X) - ncol(X), call = call('lm', formula),  weights = NULL, y = Y, x = X, model = formula, na.action = NA, qr = qr(X),terms = terms(x = formula, data = data))

  class(l) <- "lm"
  
 # Return the result 
  return(l)
}
