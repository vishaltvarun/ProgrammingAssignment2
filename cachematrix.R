## Put comments here that give an overall description of what your
## functions do

## The following script contains two functions, makeCacheMatrix and cacheSolve

## Write a short comment describing this function
## The makeCacheMatrix creates a list of functions to perform the following tasks
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the given matrix
## 4. get the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
  inversemat <- NULL
  set <- function(y) {
    x <<- y
    inversemat <<- NULL
  }
  get <- function() x
  setMatInverse <- function(matinv) inversemat <<- matinv
  getMatInverse <- function() inversemat
  list(set = set, get = get,
       setMatInverse = setMatInverse,
       getMatInverse = getMatInverse)
}


## Write a short comment describing this function
## The following function checks if the given matrix has a cached copy of its inverse in the environment.
## If the cached copy exists, it returns the cached matrix(Inverse of the given matrix). 
## Else it returns the newly computed inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversemat <- x$getMatInverse()
  if(!is.null(inversemat)) {
    message("getting cached data")
    return(inversemat)
  }
  data <- x$get()
  inversemat <- solve(data, ...)
  x$setMatInverse(inversemat)
  inversemat
}
