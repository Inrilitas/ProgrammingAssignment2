## These functions are used to calculate and then cache the 
## inverse of a matrix so that it can be looked up instead 
## of recomputed.

## This function returns a list of four functions which 
## Set the value of a matrix
## Get the value of a matrix
## Set the value of the inverse of the matrix
## Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function checks to see if there is an inverse already
## calculated for the matrix from makeCacheMatrix. if there is
## it returns the cached inverse. If not it calculates the 
## inverse and caches the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
