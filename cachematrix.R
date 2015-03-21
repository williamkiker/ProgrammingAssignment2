## The below two functions cache and compute the inverse of a given matrix.

## This first function creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y;
    inv <<- NULL;
  }
  get <- function() x
  setinv <- function(i) inv <<- i;
  getinv <- function() inv;
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This second function calculates the inverse of the special "matrix" 
## created with the above function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Getting Cached Data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
