##------------------------------------------------------------------------------
## Create a matrix object and then inverse it using the function cacheSolve.
## When the function cacheSolve executes, it'll store the results instead of
## calculating it again
##------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
      #Default value when data not cached
      xInv <- NULL
      y <- NULL
      #Set the matrix
      set <- function(y) {
            x <<- y
            xInv <<- NULL
      }
      #Get values of matrix
      get <- function() x
      #Save inverse
      setInverse <- function(inverse) xInv <<- inverse
      #Get inverted matrix
      getInverse <- function() xInv
      #Store in the lists
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##------------------------------------------------------------------------------
## Inverse a matrix created with makeCacheMatrix
## If its already cached, return it, instead of computing it
##------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
      #If the inverse was calculated, its value will be here
      xInv <- x$getInverse()
      #Check if xInv has values and return it
      if (!is.null(xInv)) {
            message("Using cached info:")
            return(xInv)
      } else {
            #Get matrix values
            m_value <- x$get()
            #Inverse of the matrix
            xInv <- solve(m_value, ...)
            #Cache the results of previous step
            x$setInverse(xInv)
            #Return results
            xInv
      }
}