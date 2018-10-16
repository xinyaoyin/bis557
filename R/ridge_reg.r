#' Fit a Ridge Regression Model
#'
#' @description This function performs ridge regression function.
#' @param formula a formula
#' @param lambda a penalty value 
#' @param data a data.frame
#' @return a ridge_reg object
#' @import MASS stats
#' @examples
#' fit <- ridge_reg(Sepal.Length ~., lambda = 1.2121, iris)
#' summary(fit)
#' @export


# Implement ridge regression function (reference to class notes)
ridge_reg = function(formula, lambda, data){
  rownames(data) <- NULL
  m <- model.matrix(formula,data)
  y <- matrix(data[,as.character(formula)[2]], ncol=1)
  y<- y[as.numeric(rownames(m)),, drop = FALSE]
 
# Use SVD to calculate the beta   
  svd_obj <- svd(m)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d

  D <- diag(svals / (svals^2 + lambda))
  
# Calculate the beta  
  beta <- V%*% D %*% t(U) %*% y

# Name coefficients
  rownames(beta)<- colnames(m)
  ret <- list(coefficients = beta, lambda = lambda, form = formula)
  class(ret) <- "ridge_reg"
  
# Return the result
  ret
}