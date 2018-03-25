## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## Define that the input x is a matrix
  inv <- NULL
  set <- function(y){
    x <<- y ## Update variable x in the containing environment
    inv <<- NULL ## Update variable inv in the containing environment
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) ## Calculate the inverse of x, if it is not existing yet
  getInverse <- function() inv ## Get inverse value in case the inverse is already calculated and stored
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() ## Search whether there exists already a value for x in the cached memory
  if(!is.null(inv)){ ## If there is cached data for x, retrieve the data
    message("getting the cached data")
    return(inv) ## Show the retrieved data
      }
  matrix <- x$get ## In case the data is not existing yet, calculate the inverse of the matrix
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
  }
