## Put comments here that give an overall description of what your
## functions do

## This function will cache the inverse of a matrix, the set and get functions will help to set and get the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {  
  i <- NULL
  setmatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of a matrix.It assumes that the matrix is always invertible.
##It checks if inverse is already computed, and if so it will cache the data instead of recomputing.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$getmatrix()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
}
