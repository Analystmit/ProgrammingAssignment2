## create a vector with a list describing the fubnction

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



## The function checks in x if the inversion was already calculated. 
##If yes, then picks it from the list, if not calculates, writes down, and returns the inverted matrix.

cacheSolve <- function(x, ...) {
      
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m     
}
