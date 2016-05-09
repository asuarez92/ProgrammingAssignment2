## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      cache <- NULL
      set <- function(y) {
            x <<- y
            cache <<- NULL
      }
      get <- function() x
      setInv <- function(inv) cache <<- inv
      getInv <- function() cache
      list(
            set = set,
            get = get,
            setInv = setInv,
            getInv = getInv
      )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
      cache <- x$getInv()
      if(!is.null(cache)) {
            message("getting cached data")
            return(cache)
      }
      m <- x$get()
      cache <- solve(m, ...)
      x$setInv(cache)
      cache
}
