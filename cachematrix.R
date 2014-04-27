## These functions cache the inverse of a matrix

## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of a matrix

cacheSolve <- function(x, ...) {
        i <- x$getInverse() ## looks if the inverse is already computated
        if(!is.null(i)) {  ## if positive just returns the value
                message("getting cached data")
                return(i)
        }
        data <- x$get() ## if there was not a cache with the inverse
        i <- solve(data, ...) ## compute the inverse
        x$setInverse(i) ## save it to the cache
        i
}