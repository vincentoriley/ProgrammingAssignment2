## This function creates a "special object" that stores a matrix and then
## caches the inverse to that matrix

## Creates a matrix and stores its inverse

makeCacheMatrix <- function(matrix = matrix()) {

  invmatrix <- NULL
  setmatrix <- function(y) {
    matrix <<- y
    invmatrix <<- NULL
  }
  getmatrix <- function() matrix
  setinv <- function(invmatrix) invmatrix <<- invmatrix
  getinv <- function() invmatrix
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinv = setinv,
       getinv = getinv)
}

## This function will return the inverse of the matrix created by 
## makeCacheMatrix

cacheSolve <- function(matrix, ...) {
        ## Return a matrix that is the inverse of 'matrix'
  
## if the invmatrix is "NULL" then calculate/return new inv
  invmatrix <- matrix$getinv()
  if(!is.null(invmatrix)) {
    message("retrieving cached inv")
    return(invmatrix)
  }
  data <- matrix$getmatrix()
  inv <- solve(data, ...)
  matrix$setinv(inv)
  inv
}
