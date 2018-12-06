#' Sparse matrix and operation functions 
#'
#' @description create a new class sparse.matrix that has add, multiply
#'              and transpose methods
#' @param i: row index of non-zero elements
#' @param j: col index of non-zero elements
#' @param x: value of the elements corresponding to i, j
#' @param dims: dimension of the sparse matrix
#' @return:  a sparse.matrix object
#' @export

# Create a new class of sparse.matrix
sparse.matrix <- function(i, j, x, dims = c(max(i), max(j))){
  structure(list(data.frame(i = c(1, 2), j = c(1, 1), x = c(3, 1)), dims), class = "sparse.matrix")
} 

#' Method of addition 
#' 
#' @description: adds two sparse matrices 
#' @param m: a sparse matrix
#' @param n: a sparse matrix
#' @return: sum of two sparse matrices 
#' @export 

# set default 
`%*%.default` = .Primitive("%*%")  
`%*%` = function(x,...){ 
  UseMethod("%*%",x)
}
`%*%` <- function(x, y) {
  UseMethod("%*%", x)
}

`+.sparse.matrix` <- function(m, n){
if (!inherits(n, "sparse.matrix"))
    stop ("input does not have a class of sparse.matrix")

if (!identical(m[[2]], n[[2]]))
    stop("matrix dimensions don't match")
    
  matrix <- merge(m[[1]], n[[1]], by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  matrix$x1[is.na(matrix$x1)] <- 0
  matrix$x2[is.na(matrix$x2)] <- 0
  matrix$x <- matrix$x1 + matrix$x2
  matrix = matrix[, c("i", "j", "x")]
  sparse.matrix(matrix$i, matrix$j, matrix$x, dims = m[[2]])
}

#' Method of multiplication 
#' 
#' @description: multiply two sparse matrices 
#' @param m: a sparse matrix
#' @param n: a sparse matrix
#' @return: product of two sparse matrices 
#' @export 

`%*%.sparse.matrix` <- function(m, n){
if (!inherits(n, "sparse.matrix"))
    stop ("input does not have a class of sparse.matrix")
	
if ((m[[2]][2] != n[[2]][1]))
    stop("matrix dimensions don't match")
      
  colnames(n[[1]]) <- c("i2", "j2", "x2")
  matrix <- merge(m[[1]], n[[1]], by.x = "j", by.y = "i2",
             all = FALSE, suffixes = c("1", "2"))
  matrix$x <- matrix$x * matrix$x2
  matrix$key <- paste(matrix$i, matrix$j, sep = "-")
  x <- tapply(matrix$x, matrix$key, sum)
  key <- strsplit(names(x), "-")
  result <- data.frame(i = sapply(key, getElement, 1),
                  j = sapply(key, getElement, 2),
                  x = as.numeric(x))
  sparse.matrix(matrix$i, matrix$j, matrix$x, dims = c(m[[2]][1], n[[2]][2]))
}

#' Method of transpose
#' 
#' @description: transpose a sparse matrix
#' @param m: a sparse matrix
#' @return: transpose of a matrix 
#' @export 

transpose <- function (x, ...) {
  UseMethod("transpose", x)
}

`t.sparse.matrix` <- function(m){
    
  l <- m[[1]]$i
  m[[1]]$i <- m[[1]]$j
  m[[1]]$j <- l
  m[[2]] <- rev(m[[2]])
  return(m)
}
