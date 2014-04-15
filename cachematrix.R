## SUMMARY: THis file contains 2 functions (makeCacheMatrix and cacheSolve) that compute the inverse of an matrix if possible storing the inverse in cache.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix: Creates an Object that can store (caches) a matrix an its inverse.
## It consists of 4 methods:
## setMatrix: set the matrix and reset the inverse to NULL
## getMatrix: get the matrix
## setInverseMatrix:set the inverse matrix for this object
## getInverseMatrix: get the inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
      #store the inverse matrix, initialize to NULL
      inverse_m <- NULL
      #inner function definition
      setMatrix <- function(y) {
            x <<- y
            inverse_m <<- NULL
      }
      #inner function definition
      getMatrix <- function() x
      #inner function definition
      setInverseMatrix <- function(inverse_matrix) inverse_m <<- inverse_matrix
      #inner function definition
      getInverseMatrix <- function() inverse_m
      
      #return a list with the methods that implement the matrix and inverse matrix setting/getting
      list(setMatrix = setMatrix, getMatrix = getMatrix,
           setInverseMatrix = setInverseMatrix,
           getInverseMatrix = getInverseMatrix)
}


## cacheSolve: Return a matrix that is the inverse of 'x'. 
## When the inverse of 'x' was already computed and x didnt change then it is returned from cache

cacheSolve <- function(x, ...) {
      #check the cache 
      inverse_m <- x$getInverseMatrix()
      # if the inverse is found in cache then use it
      if(!is.null(inverse_m)) {
            message("getting cached matrix")
            #return the inverse matrix from cache
            return(inverse_m)
      }
      #else compute the inverse and set it in cache
      else {
      mtrix <- x$getMatrix()
      inverse_m <- solve(mtrix, ...)
      x$setInverseMatrix(inverse_m)
      #return the inverse matrix
      return(inverse_m)
      }
}
