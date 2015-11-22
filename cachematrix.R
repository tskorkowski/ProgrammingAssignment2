## The aim of the following two functions is to cache the results of matrix invertion.
## To use them efficently one needs forst to create special "matrix" with makeCacheMatrix
## function and next operate on it by using cacheSolce function.

## Write a short comment describing this function
## This function creates a special "matrix" that in fact is 
## a list of functions to get and set a matrix, and get and set an inverse of this matrix.

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
## already been chached
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
  ## Return a matrix that is the inverse of 'x'
  inverse
}
