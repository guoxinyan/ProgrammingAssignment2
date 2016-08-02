## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
## However, it first checks to see if the inverse matrix has already been solved.
## If so, it gets the inverse matrix from the cache and skips the computation via the getInverse function.
## Otherwise, it proceeds to solve and compute the inverse matrix and sets the inverse matrix in the cache via the setInverse function

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if (!is.null(inv)) {
         message("getting cached data")
         return(inv)
     }
     mat <- x$get()
     inv <- solve(mat, ...)
     x$setInverse(inv)
     inv
}
