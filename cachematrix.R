## Put comments here that give an overall description of what your
## functions do

## Wraps matrix object with ability to store/retrieve cached inverse
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) x_inv <<- inv
  getinverse <- function() x_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## argument x should be an object returned from makeCacheMatrix
## Tries to retrieve cached inverse from x if it exists, otherwise
## computes it with solve and saves it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


## Example running these functions
## x <- makeCacheMatrix(matrix(c(1,-1,1,2), nrow=2, ncol=2))
## cacheSolve(x)  ## doesn't use cached inverse
## cacheSolve(x)  ## uses cached inverse
## x$set(matrix(c(4,3,3,2), nrow=2, ncol=2)) ## resets cached inverse
## cacheSolve(x)  ## recalculates inverse