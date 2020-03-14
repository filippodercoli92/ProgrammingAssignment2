## These 2 functions together take an invertible matrix and return its inverse.
## If available, the inverse matrix is retrived from the cache.

## makeCacheMatrix takes an invertible matrix "x" as input and gives as output 
## a new object which is actually a list of 4 functions to, respectively, set 
## and get the value of the matrix and set and get the value of its inverse.

makeCacheMatrix <- function(originalmat = matrix()) {
  inversemat <- NULL
  
  setMatrix <- function(x) {
    originalmat <<- x
    inversemat <<- NULL
  }
  
  getMatrix <- function() originalmat
  
  setInv <- function(inverse) inversemat <<- inverse
  
  getInv <- function() inversemat
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve calculates and returns the inverse of matrix "x". If the inverse has 
## already been calculated, then the cachesolve should retrieve the inverse from the 
## cache. 

cacheSolve <- function(x, ...) {
  # Checking the cache
  inversemat <- x$getInv()
  if(!is.null(inversemat)) {
    message("Getting cached matrix")
    return(inversemat)
  }
  
  # Getting the matrix to inverte
  originalmat <- x$getMatrix()
  
  # Calculating the inverse
  inversemat <- solve(originalmat, ...)
  
  # Setting the inverse
  x$setInv(inversemat)
  
  # Returning the inverse
  inversemat
}
