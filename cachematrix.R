## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than
## compute it repeatedly.
## Use makeCacheMatrix to create an object that
## can "wrap" a matrix cache the calculation of its inverse. 
## Use cacheSolve to compute the inverse of the special "matrix" object
## if necessary, or retrieve the inverse if it is available.

## Aside: this programmer would prefer an implementation that would
## replace what is currently getInverse with cacheSolve functionality
## inside the makeCacheMatrix function and eliminate setInverse
## and the separate cacheSolve function.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
