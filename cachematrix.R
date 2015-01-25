## These functions compute the inverse of a matrix and store it on cache
## as matrix inversion can be a costly computation.
## That avoids repeating the inversion of a given square nonsingular
## matrix by caching the result of this operation.

## Creates a special type of "matrix" object with get and set functions that
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL    
  }
  
  get <- function() x
  
  setinverse <- function(solve) m <<- solve
  
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Computes the inverse of the matrix created using the makeCacheMatrix function.
## If the inverse has already been calculated, than it will retrieve the result
## from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  ## if not already calculated, then use solve method
  if(is.null(m)) {
    m <- solve(x$get(), ...)    
    x$setinverse(m)
    return(m)
  }
  # otherwise return it from cache
  message("getting cached data")
  m
}
