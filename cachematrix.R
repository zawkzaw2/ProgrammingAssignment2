## The following codes are a pair of functions that 
## cache the inverse of a matrix.

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <-  function() x
        steInver <- function(solve.matrix) inver <<- solve.matrix
        getInver <- function() inver
        list(get = get, set = set, setInver = setInver, getInver = getInver)
}

## Compute the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInver()
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInver(inver)
        inver      
}
