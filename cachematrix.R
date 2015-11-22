## These functions cache the result of matrix inversions in order to prevent too many time-consuming computations.

## makeCacheMatrix creates a list containing functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the matrix inversion
## 4. Get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(setMat)
  {
    x <<- setMat
  }
  get <- function(){ 
    x 
  }
  setInverse <- function(solve)
  {
    inv <<- solve
  }
  getInverse <- function()
  {
    inv
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve calculates the inversion of the matrix created in the makeCacheMatrix function and caches it if 
## the inversion itself has not been already calculated and cached.

cacheSolve <- function(x, ...)
{
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
