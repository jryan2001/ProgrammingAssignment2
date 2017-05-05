## The functions below will cache the inverse of a matrix 
##rather than computing it repeatedly, provided that it is a
## square invertible matrix

## The function 'makeCacheMatrix' will create a matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invrs <<- inverse
        getinverse <- function() invrs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function 'cacheSolve' will compute the inverse 
## of the matrix that is returned with the 'makeCacheMatrix' 
## function above.

cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...)
        x$setinverse(invrs)
        invrs
}
