## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function( matrix ) {
    mx <<- matrix
    inv <<- NULL
  }
  get <- function() {
    mx
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() {
    inv
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  mx <- x$getInverse()
  if( !is.null(mx) ) {
    message("Getting cached data...")
    return(mx)
  }
  data <- x$get()
  mx <- solve(data) %*% data
  x$setInverse(mx)
  mx
}
