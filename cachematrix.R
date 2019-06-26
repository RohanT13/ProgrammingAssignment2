## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize the inverse property
  inv <- NULL
  ##Method to set the matrix
  set <- function( matrix ) {
    mx <<- matrix
    inv <<- NULL
  }
  ##Method the get the matrix
  get <- function() {
    mx
  }
  ##Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  ##Method to get the inverse of the matrix
  getInverse <- function() {
    inv
  }
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ##Return a matrix that is the inverse of 'x'
  mx <- x$getInverse()
  ##Just return the inverse if its already set
  if( !is.null(mx) ) {
    message("Getting cached data...")
    return(mx)
  }
  ## Calculate the inverse using matrix multiplication
  data <- x$get()
  mx <- solve(data) %*% data
  ## Set the inverse to the object
  x$setInverse(mx)
  ## Return the matrix
  mx
}
