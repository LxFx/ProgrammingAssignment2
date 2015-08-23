## Computing the inverse of a matrix can be a costly operation. 
## The functions below provide a system to cache the inverse of a matrix for future reference.
## This will prevent repeated calculations of the same inverse.

## The makeCacheMatrix function creates a special matrix, which is really a list containing functions to
##  - set the value of the matrix
##  - get the value of the matrix
##  - set the value of the inverse matrix
##  - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function will try to get an inversed matrix from the cache of the special matrix.
## If no inversed matrix is yet calculated, it will do so and cache the result.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
