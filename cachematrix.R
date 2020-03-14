## These 2 functions together take an invertible matrix and return its inverse.
## If available, the inverse matrix is retrived from the cache.

## makeCacheMatrix takes an invertible matrix "x" as input and gives as output 
## a new object which is actually a list of 4 functions to, respectively, set 
## and get the value of the matrix and set and get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(mean) m <<- mean
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve calculates and returns the inverse of matrix "x". If the inverse has 
## already been calculated, then the cachesolve should retrieve the inverse from the 
## cache. 

cacheSolve <- function(x, ...) {
  inversemat <- x$getSolve()
  if(!is.null(inversemat)) {
    message("Getting cached matrix")
    return(inversemat)
  }
  originalmat <- x$get()
  inversemat <- solve(originalmat, ...)
  x$setSolve(inversemat)
  inversemat
}
