## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Compute the inverse of the matrix using caching
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  determinant <- det(data)
  if (determinant == 0) {
    stop("Matrix is singular and cannot be inverted.")
  }
  
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}