## The following codes are a pair of functions that 
## cache the inverse of a matrix.

## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        cacheInverse <- matrix(0, nrow(x), ncol(x))
        for (i in 1:nrow(x)) {
                cacheInverse[i,] <<- solve(x[i,], cacheInverse)
        }
        return(cacheInverse)
}

## Compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
        if (!is.null(cacheInverse)) {
                ## Return a matrix that is the inverse of 'x'
                return(cacheInverse)
        }
        y <<- solve(x, NULL)
        cacheInverse <- matrix(y, nrow(y), ncol(y))
        return(cacheInverse)
}

