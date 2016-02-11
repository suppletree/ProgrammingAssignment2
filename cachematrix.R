## The below functions take advantage of R's lexical scoping to cache the inverse of a matrix, 
## so that the inverse does not have to be computed again, if the matrix has not changed.

##  makeCacheMatrix is a function that returns a list containing functions to:
## (1) set the value of the matrix (2) get the value of the matrix
## (3) set the inverse of the matrix (4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Below function first checks to see if the inverse of the matrix (beign passed) is already cached.
## If it is cached, it gets the cached calue, if not - it determines the inverse of the matrix and 
## caches it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
