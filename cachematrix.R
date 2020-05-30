# Creates an object that contains
# a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(y) i <<- y
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Returns the cached inverse of a matrix if it exists,
# otherwise, computes, caches, and return the inverse.
cacheSolve <- function(x, ...) {
        m <- x$get()
        i <- x$getInverse()
        if (is.null(i)) {
                x$setInverse(solve(m))
                i <- x$getInverse()
        }
        i
}
