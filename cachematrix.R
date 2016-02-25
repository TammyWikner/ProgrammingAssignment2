
## The following functions retrieve the inverse of a matrix from the cache if the 
## inverse has previously been computed, else it computes the inverse and stores 
## the inverse in the cache. An assumption is made that the input matrix is always
## invertible.


## makeCacheMatrix() - Passed a matrix ('x'). Defines four functions to store and return the matrix, 
##                     cache the inverse of the matrix and return this cached inverse matrix.
##                     Returns a special "matrix" object which is really a list containing these functions.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) i <<- inv
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve() - Retrieves the inverse value of the special "matrix" from the cache. 
##                If the inverse has already been computed (and the matrix has not changed),
##                then cacheSolve() returns this inverse from the cache.
##                If the inverse has not been computed/cached, then cacheSolve() computes  
##                the inverse, stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()     
        if(!is.null(i)){       
                message("getting cached data")
                return(i)      
        }
        # a computed inverse has not been cached yet so ...
        data <- x$get()         
        i <- solve(data,...)    # solve() returns the inverse of data
        x$setInverse(i)        
        i
}
