
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
  # Your code here.

X = model.matrix(formula, data)
Y = data$Sepal.Length
 
 my_lm = function(Y, X) {
  beta <- solve((t(X) %*% X)) %*% t(X) %*% Y
  sigma_squared = sum((Y - X%*%beta)^2)/(nrow(X) - ncol(X))
  XTX = t(X) %*% X
  XTXI = diag(solve(XTX))
  se_beta = sqrt(XTXI * sigma_squared) 
  t_score = (beta)/(se_beta)
  p_value = 2 * pt(-t_score, df = nrow(X) - ncol(X))
  result = cbind(beta,se_beta,t_score,p_value)
  colnames(result) = c("Estimate", "Std. Error",   "t value","Pr(>|t|)")
  return(result)  
}
}
