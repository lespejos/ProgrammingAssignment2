## The "makeCacheMatrix" function creates a special "matrix".

## Functions do:
##1.Set the value of the matrix using the function "set".
##2.Get the value of the matrix using the function "get".
##3.Set the value of the inverse of the matrix using the function "setinverse".
##3.Get the value of the inverse of the matrix using the function "getinverse".

## Once created this matrix is storage in the cache, 
## so it can be used inside of an R object without recalculating it, if the values of the matrix do not change. 

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y)
  {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) minv <<- inverse
  getinverse <- function() minv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}



## The "cacheSolve" function verifies if the inverse of the matrix 
## has been computed and storage in the cache.
## If the inverse of the matrix was not previously calculated, 
## it computes the inverse using the "solve" function
## and storages the values using "setinverse" in the cache.
## Therefore the inverse of the matrix can be used inside of an R object without recomputing it.


cacheSolve <- function(x, ...) {
  minv <- x$getinverse()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinverse(minv)
  minv
}
