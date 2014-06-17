## Functions for the creation and manipulation of matrices with cached inverse
## calculations. `makeCacheMatrix` takes a matrix and returns an object with
## methods for getting and setting the matrix or its inverse. `cacheSolve`
## returns the inverse of a matrix object constructed using `makeCacheMatrix`.
## Note that it should not be necessary for you to get/set the inverse of the
## matrix wrapped by the object `makeCacheMatrix` returns. `cacheSolve` will
## handle this for you.

## Given a matrix as input, returns an object which "wraps" the matrix input and
## provides methods for getting/setting the value of the matrix or its inverse.
## Setting the value of the matrix automatically clears the value of the inverse.
## Note that you should not need to get/set the inverse directly; use the
## `cacheSolve` function instead.
makeCacheMatrix <- function(x = matrix()) {
  # Cached inverse
  i <- NULL
  
  # Set the value of the enclosed matrix
  set <- function(y) {
    x <<- y
    i <<- NULL ## Clear the cached inverse
  }
  
  # Get the value of the enclosed matrix
  get <- function() x
  
  # Cache the value of the inverse. You should not call this method yourself.
  # Use `cacheSolve` instead.
  setinverse <- function(inverse) i <<- inverse
  
  # Get the value of the cached inverse. You should not call this method
  # yourself. Use `cacheSolve` instead.
  getinverse <- function() i
  
  # Return a new "matrix object" which is a list of methods interacting with the
  # matrix and inverse via closure
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Given a "matrix object" as input, returns the inverse. "Matrix objects" are
## the return values of `makeCacheMatrix`. `cacheSolve` checks these objects to
## see if their inverses have been calculated before. If so, `cacheSolve` simply
## returns the values it finds. If no value is found, `cacheSolve` calculates
## the matrix inverse and gives it to the matrix object for caching. The
## getinverse and setinverse methods provided by the matrix object are for
## `cacheSolve` use only; do not call them yourself in order to circumvent
## `cacheSolve`.
cacheSolve <- function(x, ...) {
  ## Check to see if the value is already cached
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  ## Use the solve method to get the inverse of the matrix. Note that we assume
  ## the matrix will have an inverse.
  i <- solve(data, ...)
  ## Cache the result
  x$setinverse(i)
  i
}