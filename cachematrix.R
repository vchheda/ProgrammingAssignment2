## The below function is used to return the Inverse of a square Matrix and cache it.
## First function will cache the matrix
## Second function computes the inverse of the matrix.

## makeCacheMatrix function creates a special vector which is a list containing 
## a function to set and get the value of Matrix and set and get the value of 
## inverse of Matrix
## ASSUMPTION - the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
      inverseMatrix <- NULL
      set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
      }
      get <- function() x
      setinverse <- function(invmat) inverseMatrix <<- invmat
      getinverse <- function() inverseMatrix
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)      
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverseMatrix <- x$getinverse()
      if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
      }
      data <- x$get()
      inverseMatrix <- solve(data, ...)
      x$setinverse(inverseMatrix)
      inverseMatrix
}