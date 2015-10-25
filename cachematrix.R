## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list of functions to get and set a matrix, and get and set an inverse of this matrix
makeCacheMatrix <- function(x = matrix())
{
  inverse <- NULL
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get , setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## The following fuction either calculates an inverse of a ginven matrix or reads it from memory if it has
## already been chached or has changed
cacheSolve <- function(x, ...)
{
  inverse <- x$getInverse()
  if(!is.null(inverse) )
  {
    message("getting chached inversed matrix")
    return(inverse)
  }
  inverse <- solve(x$get())
  x$setInverse(inverse)
  inverse
  ## Return a matrix that is the inverse of 'x'
}
