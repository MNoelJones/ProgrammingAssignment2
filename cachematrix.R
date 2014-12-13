## Matrix inverse caching

## Generator function to hold a matrix 'x' and its 'inverse' - possibly cached
makeCacheMatrix <- function(x = matrix()) {
  # Set the inverse initially NULL to allow testing of whether it has already been set or not
  inverse <- NULL
  # Setter Function for matrix
  set <- function(y) {
    # assign the matrix from the parameter provided
    x <<- y
    # reset the inverse if the matrix is changed
    inverse <<- NULL
  }
  # Getter funciton for matrix
  get <- function() x
  # Setter function for matrix inverse
  setinverse <- function(inv) inverse <<- inv
  # Getter function for matrix inverse
  getinverse <- function() inverse
  # return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculate or retrieve the cached inverse of the matrix 'x'
cacheSolve <- function(x, ...) {
  # Query the cached matrix inverse...
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
  } else {
    # ... otherwise, retrieve the matrix,
    data <- x$get()
    # compute its inverse,
    inv <- solve(data, ...)
    # and store in the cache.
    x$setinverse(inv)
  }
  ## Return the computed inverse of 'x'
  inv
}
