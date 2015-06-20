## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # create a new NULL cache
  inv <- NULL
  # set the value of the matrix
  set <- function(mtx) {
    x <<- mtx # update the matrix
    inv <<- NULL # if new value is set, clean the cache
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse
  setInverse <- function(inverse) inv <<- inverse
  # get the value of the inverse
  getInverse <- function() inv
  # return the functions list object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  # check if the inverse data was cached
  if(!is.null(inv)) {
    # if yes, notify user and return the cached version
    message("getting cached data")
    return(inv)
  }
  # if not, get the original matrix 
  mtx <- x$get()
  # calculate the inverse matrix with 'solve' and additional parameters
  inv <- solve(mtx, ...)
  # cache the inversed matrix
  x$setInverse(inv)
  # return a matrix that is the inverse of 'x'
  inv
}
