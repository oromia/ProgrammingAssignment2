## This function creates a special "matrix" object
## that can cache its inverse.

## makeCacheMatrix : This function takes an invertible 
## matrix as its input. It contains functions to:
##  1. set(), sets matrix and clears inverse
##  2. get(), returns matrix
##  3. setinverse(), sets inverse matrix
##  4. getinverse(), gets inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <-  function(y){  
              x <<- y
              m << NULL
          }
  get <-  function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve : This function returns inverse of a matrix.
## It checks if inverse has already been performed. if
## calculated, cache will be retrieved
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  # if cache not null, return 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # else get matrix, do inverse and cache
  mydata <- x$get()
  m <- solve(mydata, ...)
  x$setinv(m)
  m
}
