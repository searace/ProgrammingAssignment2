## this is first r programming to submit into github
## Description: a pair of functions that cache the inverse of a matrix.
## Date: 12/06/2015

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  #initial cache should be empty
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()                     ##getting the inverse
  if(!is.null(i)) {                       ## checking for the presence of inverse
    message("getting cached data")   
    return(i)
  }
  m <- x$get()                            ##getting matrix
  i <- solve(m, ...)
  x$setinverse(i)
  i
}