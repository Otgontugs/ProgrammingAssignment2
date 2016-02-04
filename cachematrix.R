## Put comments here that give an overall description of what your
## functions do

## creates an object to hold a matrix cache

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setCachedInverse <- function(inv) cachedInverse <<- inv
  getCachedInverse <- function() cachedInverse
  list(set = set, get = get,
       setCachedInverse = setCachedInverse,
       getCachedInverse = getCachedInverse)
}


## Comments are written inline

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getCachedInverse()
  if(!is.null(m)) {     ## we got it from cache
    message("getting cached reverse matrix")
    return(m)           ## Return reverse from cache
  }
  data <- x$get()
  m <- solve(data)      ## Calculate reverse matrix
  x$setCachedInverse(m) ## Store matrix to the cache
  m                     ## return reverse
}
