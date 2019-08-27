## The following functions compute the inverse of a matrix, after checking
## if the contents of the matrix have changed or not. In case the contents
## do not change, the functions retrieve the existing value of the inverse 
## which has been computed before and store as a cache. If the contents 
## change, the functions compute the required inverse.

## makeCacheMatrix creates a special matrix object that can save it's inverse
## matrix as a cache. The contents of the matrix can be set or retrieved using 
## the set and get. setInverse and getInverse set and display the value of the 
## inverse matrix respectively.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(inv) inverse <<- inv
      getInverse <- function() inverse
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve computes the inverse of the matrix returned by the previous
## function. If the inverse has alredy been calculated or set, the function
## simply fetches the inverse matrix. Otherwise, it computes the inverse.

cacheSolve <- function(x, ...) {
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      x_matrix<- x$get()
      inverse <- solve(x_matrix, ...)
      x$setInverse(inverse)
      inverse
}

