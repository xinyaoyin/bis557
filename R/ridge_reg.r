#' Fit a Ridge Regression Model
#'
#' @description This function passes parameters to the ridge_reg function.
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
   
  svd_obj <- svd(m)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d

  D <- diag(svals / (svals^2 + lambda))
  beta <- V%*% D %*% t(U) %*% y
  rownames(beta)<- colnames(m)
  ret <- list(coefficients = beta, lambda = lambda, form = formula)
  class(ret) <- "ridge_reg"
  ret
}