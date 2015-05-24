## cachematrix.R
## 
## Solve inverse of the matrix
## If we need to reuse the inverse of this,
## the cacheSolve returns the inverse
## without recalculating it.
## 
## How to use it:
## Assume you have a square matrix (x)
## 1. create a cached matrix
##    cachedMatrix <- makeCacheMatrix(x)
## 2. find the inverse of the cachedMatrix
##    cacheSolve(cachedMatrix)
##
## If you need to use the inverse of the matrix (x),
## calling cacheSolve, again, will not recalculate
## the inverse, but just return the result. If the
## square matrix (x) changes, repeat the 1 and 2.

## Create a matrix object that will allow
## the inverse to be cached.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the inverse matrix of 
## the matrix object created from the
## makeCacheMatrix function.
## If the inverse has been cached, 
##   return the cached value.
## If not, calculate the inverse,
##   then cache it for later use.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
        
}
