

## Creates a special matrix object that can cache its inverse, implemented as
## a list containing the following functions (or methods):
##  - set: set the value of the matrix
##  - get: get the value of the matrix
##  - setinverse: set the value of the inverse matrix
##  - getinverse: get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_inv <<- inverse
  getinverse <- function() x_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of a matrix object created by makeCacheMatrix,
## making use of the latter's cache to avoid unnecessary recomputations.
cacheSolve <- function(x, ...) {
  x_inv <- x$getinverse()
  if (!is.null(x_inv)) {
    message("getting cached inverse")
    return(x_inv)
  }
  # Compute the inverse
  x_inv <- solve(x$get())
  
  # Store the inverse in the cache
  x$setinverse(x_inv)
  x_inv
}
