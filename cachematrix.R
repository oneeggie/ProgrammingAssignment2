## Coursera - Caching the Inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrix_inv <<- inverse
  getinverse <- function() matrix_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
  
## cacheSolve calculates the mean of the inverse matrix created
## with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
    matrix_inv <- x$getinverse()
  if (!is.null(matrix_inv)) {
    message("getting cached data")
    return(matrix_inv)
  }
  d_matrix <- x$get()
  matrix_inv <- solve(d_matrix, ...)
  x$setinverse(matrix_inv)
  matrix_inv
}
  
  