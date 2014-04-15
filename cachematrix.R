## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inverse_m <- NULL
      setMatrix <- function(y) {
            x <<- y
            inverse_m <<- NULL
      }
      getMatrix <- function() x
      setInverseMatrix <- function(inverse_matrix) inverse_m <<- inverse_matrix
      getInverseMatrix <- function() inverse_m
      list(setMatrix = setMatrix, getMatrix = getMatrix,
           setInverseMatrix = setInverseMatrix,
           getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse_m <- x$getInverseMatrix()
      if(!is.null(inverse_m)) {
            message("getting cached matrix")
            return(inverse_m)
      }
      mtrix <- x$getMatrix()
      inverse_m <- solve(mtrix, ...)
      x$setInverseMatrix(inverse_m)
      inverse_m
}
