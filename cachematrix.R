## Assignment: Caching the Inverse of a Matrix

## This function creates a special “matrix” object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inverse) {
    inv <<- inverse
  }
  return(list(
    set = set,
    get = get,
    getinverse = getinv,
    setinverse = setinv
  ))

}


## This function computes the inverse of the special “matrix” 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  m <- solve(x$get())
  x$setinverse(m)
  #return(m)
        ## Return a matrix that is the inverse of 'x'
}
