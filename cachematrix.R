## makeCacheMatrix creates a special matrix object that can cache its inverse
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, returns the inverse from the cache.

## Creates a special matrix, which is really a list of functions to
## - set the value of the matrix
## - get the value of the matrix
## - set the inverse of the matrix
## - get the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates the inverse of the matrix created with the previous function.
## Checks first if the inverse has already been calculated. If yes, it is returned from the cache,
## else calculates the inverse, saves it to the cache and then returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
