## Matrix inversion is computation intensive, therefore caching it is beneficial 
## rather than computing it repeatedly.

## Caching Inverse of a Matrix:

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This computes inverse of the "matrix" created with makeCacheMatrix 
## In case inverse has already been calculated and is not changed then it would just retrieved that from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
