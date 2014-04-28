## cachematrix.R
## ----------------------------------------------------------------------------
## This file contains two functions
## 1. makeCacheMatrix - To Create a Matrix Object
## 2. cacheSolve - To Calculate the inverse of the matrix using 'solve' function
## ----------------------------------------------------------------------------
# This function creates a matrix object
makeCacheMatrix <- function(x = matrix()) {
  invs_of_x <- NULL
  set <- function(y) {
    x <<- y
    invs_of_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs_of_x <<- inverse
  getinverse <- function() invs_of_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function cacheSolve returns the inverse of the matrix created with the makeCacheMatrix function
## If the inverse is already cached, it will return instead of recalculate
cacheSolve <- function(x, ...) {
  invs_of_x <- x$getinverse()
  if (!is.null(invs_of_x)) {
    message("Retrieving cached inverse of the matrix x")
    return(invs_of_x)
  } else {
    invs_of_x <- solve(x$get())
    x$setinverse(invs_of_x)
    return(invs_of_x)
  }
}