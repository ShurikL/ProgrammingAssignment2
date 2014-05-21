# R programming course - Programming Assignment 2
#
# The functions cache the inverse of a matrix.


# The function creates special "vector", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(invrs) inv <<- invrs
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# The function computes the inverse matrix for the matrix created by makeCacheMatrix fuction.
# It first checks if the inverse matrix has already been computed. If so, it gets the
#inverse matrix from the cashe and skips the computing. Otherwise, it computes 
# the inverse matrix using the function solve() and sets the value of inverse matrix in the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
