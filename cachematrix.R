## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        return(x)
    }
    setInverse <- function(pInverse) {
        inverse <<- pInverse
    }
    getInverse <- function() {
        return(inverse)
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then cacheSolve should retrieve the 
##  inverse from the cache
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        print("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    return(inverse)
}