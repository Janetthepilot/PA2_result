## Put comments here that give an overall description of what your
## functions do

## Get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function() invrs <<- solve(x)
  getinverse <- function() invrs
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Cache the matrix's inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
      message("getting cached data")
      return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinverse(invrs)
  invrs
}
