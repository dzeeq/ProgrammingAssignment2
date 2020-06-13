## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  ## cached inverse of matrix x
  inverse <- NULL
  
  ## function setting new matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## function getting set matrix
  get <- function() x
  
  ## function setting cached inverse of the matrix
  setInverse <- function(i) {
    inverse <<- i
  }
  
  ## function getting cached inverse of the matrix
  getInverse <- function() inverse
  
  ## returning list of functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## trying to get cached inverse matrix...
  inverse <- x$getInverse()

  ## if it is set returning it ...
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  ## otherwise getting matrix data and calculating the inverse, 
  ## setting it into cache ...
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  
  ## returning inverse
  inverse
}
