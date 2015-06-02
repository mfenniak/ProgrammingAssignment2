# Contains functions that provide the ability to calculate the inverse of a
# matrix, and store that calculation in a cache so that it can be re-used
# later if it has already been calculated.  The general form of using this
# would be to invoke makeCacheMatrix, passing in your matrix, and store that
# value.  Whenever you need to access the inverse of the matrix, invoke
# cacheSolve; the first time it is invoked it will solve the matrix, and
# subsequent calls will re-use the same solution for speed.

# Create a wrapper around a matrix that is capable of calculating and caching
# the inverse of the matrix.  The underlying matrix can be accessed and
# changed with the get and set functions.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setSolution <- function(newInverse) inverse <<- newInverse
    getSolution <- function() inverse
    list(set = set,
         get = get,
         setSolution = setSolution,
         getSolution = getSolution)
}

# Calculate the inverse of the matrix, or, return the pre-calculated and
# cached solution if already available.
cacheSolve <- function(x, ...) {
    inv <- x$getSolution()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setSolution(inv)
    inv
}
