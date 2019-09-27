## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse_mtx <- NULL
  set <- function(y) {
    x <<- y
    inverse_mtx <<- NULL
  }
  get <- function() x
  setinverse <- function(im) inverse_mtx <<- im
  getinverse <- function() inverse_mtx
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and
# the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse_mtx <- x$getinverse()
  
  if(!is.null(inverse_mtx)) {
    message("Will use cached data")
    return(inverse_mtx)
  }

  data <- x$get()
  inverse_mtx <- solve(data, ...)
  x$setinverse(inverse_mtx)
  inverse_mtx
}
