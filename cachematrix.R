
## Creates a wrapper around a a matrix

makeCacheMatrix <- function(input = matrix()) {
  inverse <- NULL
  set <- function(y) {
    input <<- x
    inverse <<- NULL
  }

  get <- function() {
    input
  }

  setInverse <- function(inv) {
    inverse <<- inv
  }

  getInverse <- function() {
    inverse
  }

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns inverse of matrix 'x' from the cache if available
## Otherwise computes and caches the inverse

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  input <- x$get()
  inverse <- solve(input, ...)
  x$setInverse(inverse)
  inverse
}
