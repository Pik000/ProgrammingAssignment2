## This script creates a matrix, and solves the invese of this matrix.
## It then stores the inverse of this matrix.
## The second part of the script checks to see if there is an inverse
## matrix already stored at x$getInverse. If it is stored then it is displayed,
## otherwise it is caculated.

## This script takes in the argument of a matrix, and it is stored within the 
## function 'set'. setInverse when called caculates the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) #caculates the inverse of the matrix.
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This part of the script checks to see if there is an inverse
## matrix already stored at x$getInverse. If it is stored then it is displayed,
## otherwise it is caculated and then displayed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("retrieving inverse matrix")
    return(inv)
  }
  ma <- x$get()
  inv <- solve(ma,...)
  x$setInverse(inv)
  inv
  }
