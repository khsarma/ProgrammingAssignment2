## 2 functions to cache the inverse of matrix

## makeCacheMatrix function creates a special matrix object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
            x <<- y
            m <<- NULL
          }
          get <- function() x
          setinv <- function(inverse) m <<- inverse
          getinv <- function() m
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)
}


## cacheSolve function computes the inverse of matrix returned
## by makeCacheMatrix. If the inverse is already available, 
## function will pull the cached version of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if (!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
