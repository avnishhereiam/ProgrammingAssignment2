## Matrix to solve inverted matrix repeatedly.
## 

##created a invertible matrix and chaches it.
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## takes the matrix if not inverted then inverts it from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invFun <- x$getInverse()
  if(!is.null(invFun)) {
    message("getting cached data")
    return(invFun)
  }
  data <- x$get()
  invFun <- solve(data, ...)
  x$setInverse(invFun)
  invFun
}