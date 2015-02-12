## These functions compute the inverse of a matrix, using a cache in order
## to avoid multiple computations when the matrix has not changed

## Creates an object that contains the information of a matrix and its inverse
## with methods to update their values. In case we update the matrix with
## a new one, the inverse is set to NULL to indicate that it must be
## computed again
##
## returns a list object "obj" with methods obj$get(), obj$set(matrix),
##         obj$getInverse() and obj$setInverse(matrix)
makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL
    set <- function(y) {
        # if the matrix are identical, there is no need to update the cache
        if (!identical(x,y)) {
            x <<- y
            inverse <<- NULL
        }
    }
    get <- function() x
    setInverse <- function(newInverse) inverse <<- newInverse
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Receives a cacheMatrix object created with makeCacheMatrix and computes
## its inverse if necessary. It first checks if the inverse is already
## in the cache.
##
## return the inverse matrix and updates the cacheMatrix object x with the
## inverse for further computations
cacheSolve <- function(x, ...) {
    cacheInverse <- x$getInverse()
    if (is.null(cacheInverse)) { # the inverse is not computed
        cacheMatrix <- x$get()
        # in order to pass the ... arguments, we need to specify a matrix b
        # (see ?solve) and if b is the identity matrix, the solve computes
        # the inverse
        identityMatrix <- diag(nrow(cacheMatrix))
        cacheInverse <- solve(cacheMatrix, identityMatrix, ...)
        x$setInverse(cacheInverse)
    } else { # the inverse is cached
        message("getting cached inverse")
    }
    cacheInverse
}
