
## This file implemented a cached version of a funciton to compute matrix inversion.
## If the input matrix has not changed since the last time the inverse was 
## calculated, we do not recompute the inverse and instead return the cached
## version.  If the matrix does change, we will recompute the inverse.

## makeCacheMatrix(matrix)
## wrapper around a standard R matrix, proving getter and setter methods,
## and getinverse and setinverse methods to store a cached version of the 
## inverse.  If the underlying matrix is changed via its setter, the cached
## inverse is invalidated

makeCacheMatrix <- function(m = matrix()) 
{
  inverse <- NULL # inverse the internal variables in Function
  
  ## set(matrix)
  ## sets the matrix and invalidates the cached inverse
  set <- function(y) 
  {
    m <<- y
    inverse <<- NULL
  }
  
  ## get()
  ## return the matrix
  get <- function()
  {
    m
  }
  
  ## setinverse(matrix)
  ## sets the value of the inverse of the matrix
  setinverse <- function(inv) 
  {
    inverse <<- inv
  }
  
  ## return the cached value of the inverse of the matrix.
  getinverse <- function() 
  {
    inverse
  }
  
  ## return a list of getter and setter methods as a result of object creation
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)    
}


## cacheSolve(cacheMatrix, ...)
## Returns the inverse of the cacheMatrix object
## If the inverse has already been calculated and the matrix not changed
## since the last invocation, returns the cached version.
## If the inverse has not yet been calculated or the underlying matrix 
## changed since the last call, (re)computes the value with a call to solve(...)

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'm'
  inv <- m$getinverse()
  if(!is.null(inv)) 
  {
    ## Inverse already calculated, value not stale
    message("getting cached data")
    return(inv)
  }
  
  ## Either inverse has never been calculated or cached inverse is stale
  data <- m$get()
  inv <- solve(data, ...)
  m$setinverse(inv)
  inv    
}