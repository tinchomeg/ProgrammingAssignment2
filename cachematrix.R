## The functions makeCacheMatrix and cacheSolve allow to store a matrix
## with a cache for its inverse, reducing the time to get the inverse
## if the values of the matrix have not been changed.

## Creates a list containing a matrix and a cache for its inverse.
## Also, it contains getter/setter functions for the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the matrix.
## If it has been cached, it returns the value.
## Otherwise, it calculates the inverse and stores it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
