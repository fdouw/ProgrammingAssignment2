
## Floris Douw
## 2014

## Programming Assignment 2 for the R Programming course on Coursera.
##
## These functions allow you to create matrices with cacheable inverses and to
## get these inverses either from computation or from cache if possible.




makeCacheMatrix <- function(x = matrix(1:4, 2, 2)) {
    ## x is an invertible matrix
    ##
    ## Returns an object containing the matrix and a cache for the inverse.  The
    ## inverse has not yet been computed.
    inverse <- NULL
    set <- function(n) {
        x <<- n
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
    ## x is a 'CacheMatrix' as returned by makeCacheMatrix.
    ## Further parameters will be passed on to the solve() function.
    ##
    ## Returns the inverse of the matrix in x.  If this had been computed yet,
    ## then it will be stored in x as well.
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached inverse.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}