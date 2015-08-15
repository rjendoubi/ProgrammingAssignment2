## cachematrix.R: cache the result of matrix inversion operations

## Matrix inversion can be an expensive process. These functions
## allow you to cache (save) the result of an inversion for a given
## matrix when it is calculated, and subsequently retrieve the same
## results again directly from the cache, avoiding recalculation.

## The 'makeCacheMatrix' function creates a special data structure
## to contain your matrix and its cached inversion.
## The 'cacheSolve' function works with this data structure to
## find the inverted matrix (using solve()), and subsequently
## retrieve the cached version if available.

## Note that if the matrix is changed or updated then the cached
## version will be invalidated and will require recalculation.


## makeCacheMatrix
## Arguments: a square matrix
## Returns: data structure able to hold a matrix and its inverse,
##     for use with cacheSolve (see below). Structure is a list
##     named elements get, set, getinverse and setinverse, each
##     containing a function for interacting with the contained
##     matrix / inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    get <- function () x
    setinverse <- function(z) m <<- z
    getinverse <- function() m

    list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve
## Arguments: a cached matrix structure as returned by makeCacheMatrix
## Returns: the inverse of the matrix in the input structure. If the
##     inverse has been calculated previously, the result is retrieved
##     directly from the cache; if not, cacheSolve will calculate the
##     inverse (using solve()) and cache the result for future retrieval.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("Getting cached data")
        return(m)
    }

    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
