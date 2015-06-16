# This is the .R file for the Programming Assignment 2


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # initialize the inversed variable
        inv <- NULL
        
        # the set function to set a new matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # the get function to get the matrix
        get <- function() { x }
        
        # the setInverse function to set the inversed matrix to the inv variable
        setInverse <- function(inverse) { inv <<- inverse }
        
        # the getInverse function to get the inversed matrix value of the inv variable
        getInverse <- function() { inv }
        
        # return the list of functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        
        # get the matrix and solve for inverse matrix
        data <- x$get()
        inv <- solve(data, ...)
        
        # set the inverse matrix into cache
        x$setInverse(inv)
        
        # return the inverse matrix
        inv
}
