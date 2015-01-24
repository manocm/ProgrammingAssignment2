## These functions make a cache of the inverse of a matrix
## and retrieve it this cache if it exists

## This function create the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function retrieves the cache if it exists

cachesolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else{
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    return(m)
  }
}

## example

## mtx <- matrix(rnorm(9), 3, 3)
## test <- makeCacheMatrix(mtx)
## cachesolve(test)
