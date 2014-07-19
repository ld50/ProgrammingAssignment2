## File creates two functions, the first makeCacheMatrix makes
# a special cachable matrix like list. The inverse of this matrix
# like list can be cached. The second function, cacheSolve, uses
# the matrix like list structure and either returns a cached inverse
# or inverts the matrix, caches the result, then returns the result.
# The goal is to only do expensive calculations once.


## make a matrix which is actually a list of functions that
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse (cache)
# get the value of the inverse (read the cache)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## when called on a makeCacheMatrix matrix this function will solve that
# matrix of and cache the result or return the already cached result

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
