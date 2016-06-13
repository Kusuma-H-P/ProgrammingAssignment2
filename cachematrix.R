## Caching the Inverse of the Matrix:
## First function describes thr creation of the matrix to cache the inverse.
## Second function computes the Inverse of the Matrix, only if the makeCacheMatrix doesnot retrive the Inverse of the matrix.

## This function creates a cache matrix that provide its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function()x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

## This function computes a inverse of the matrix created by makeCacheMatrix.
## If inverse has already been created by above function then it retrieves the inverse from the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse
  
  if(!is.null(inv)){
      message("getting cached data")
      return(inv)
  }
  
  mat <- x$get()
  
  inv <- solve(mat, ...)
  
  x$setInverse(inv)
  
  inv
  }
