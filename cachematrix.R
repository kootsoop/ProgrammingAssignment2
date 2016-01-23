## The functions in this file allow calculation of the inverse of a matrix to be re-used from
## a cached version if the calculation is to be called many times.
##
## The usage of these two functions is:
##
##    x <- matrix(c(1,2,3,4),2,2) # Any matrix x
##    xC <- makeCacheMatrix(x) # Form the cached version of x
##    xInv <- cacheSolve(xC)
##
## (C) Peter J. Kootsookos 2016/01/23

## makeCacheMatrix creates a matrix data type that allows a cached version of the matrix's
## inverse to be stored as well as the matrix itself.
##
## This function doesn't actually calculate the inverse for us, the next function, cacheSolve,
## does that.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(newinverse) inverse <<- newinverse
  getinverse <- function() inverse
  # Return an object with these functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a variant of the solve() function that can be used
## to calculate the inverse of a matrix. cacheSolve adds the abiltiy to 
## cache the inverse, to that it is only calculated once for the same
## starting matrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

message("Running a test example.")
x <- matrix(c(1,2,3,4),2,2) 
xC <- makeCacheMatrix(x) 
xInv <- cacheSolve(xC)
xInv2 <- cacheSolve(xC) # This should output "getting cached inverse" if the caching was successful.

xC$set(x+1)
xInv <- cacheSolve(xC)  # Because we changed the matri we are interested in, we need to recalculate
                        # the cached inverse, so this call should NOT print out the message.
xInv2 <- cacheSolve(xC) # This should output "getting cached inverse" if the caching was successful.

