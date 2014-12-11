## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly.


## Creates a list of functions that
## can cache the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
	# sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
      m <- NULL

	#set the value of the matrix
	set <- function(y) {
          ## caches the inputted matrix so that cacheSolve can check 
          ##whether it has changed (note this is within the setmatrix function)
          x <<- y

          ##sets the value of m (the matrix inverse if used cacheSolve) to NULL
          m <<- NULL
      }

      ##set the value of the matrix
      get <- function() x
      setInverse <- function(inverse) m <<-inverse
	getInverse <- function() m
	
      ##creates a list to hold the four functions
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)

  }
}

## Return a matrix that is the inverse of 'x'
## Computes the inverse of the matrix returned by makeCacheMatrix(). 
## If the inverse has already been calculated it retrieves from the cache.
cacheSolve <- function(x, ...) {
      m <- x$getInverse()
   
      ##if the matrix has already been populated return it
      ##without repopulating and recalculating
      if ( ! is.null(m)) {
          print("getting cached data")
          return(m)
      }

      ##if the matrix has not been populated, poulate it
      ##set the inverse and return it
      m <- solve(x$get())
      x$setInverse(m)
      m
}

##Test cases, please uncomment to run.
##a <- makeCacheMatrix(matrix(1:4,2))
##a$get()
##a$getInverse()
##a$set(matrix(5:8,2))
##a$get()
##cacheSolve(a)
##cacheSolve(a)
##a$getInverse()
##b = a$getInverse()
##a$get() %*% b