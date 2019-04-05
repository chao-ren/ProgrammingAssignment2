## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix - creates a new object and initializes it with the matrix passed in x
## The object provides 4 functions:
## (1) set - sets the matrix value of the object
## (2) get - returns the matrix value of the object
## (3) setinv - sets the value of the inverse cache
## (4) getinv - gets the value of the inverse cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  else {
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
  }
  i
}
