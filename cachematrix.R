## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function (y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(y) inverse <<- y
    
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## this function takes the "matrix" object that the above function returns and 
## returns the inverse (return the cached inverse or compute if not already calculated )

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
