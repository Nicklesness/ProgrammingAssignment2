## Example matrix m <- matrix(1:4, nrow = 2, ncol = 2)
## Works like an example cachemean

## Reccomends: assign "makeCacheMatrix" to some variable "V" to activate all functions inside the list
## USE:  V$setValue(m) to store matrix
## USE:  V$setCache(m) to store inverse
## USE:  V$getValue(m) to get matrix
## USE:  V$getCache(m) to get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## set matrix to NULL
        invMatrix <- NULL
        ## set Matrix
        setValue <- function(y = matrix){
                x <<- y
                invMatrix <<- NULL
        }
        ## get Matrix
        getValue <- function() x
        ## set invrse of the Matrix
        setCache <- function(x) {
                invMatrix <<- solve(x)
        }
        ## get inverse of the Matrix
        getCache <- function() invMatrix
        
        ## set list of functions to get and set 
        list(setValue = setValue, getValue = getValue, setCache = setCache, getCache = getCache)
}




## USE:  cacheSolve(V)
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## take value of invMatrix
        invMatrix <- x$getCache()
        ## Check if invMatrix is not NULL print getting cached data
        if(!is.null(invMatrix)){
                message("getting cached data")
                return(invMatrix)
        }
        
        ## set up new Inverse Matrix
        data <- x$getValue()
        invMatrix <- solve(data)
        x$setCache(invMatrix)
        invMatrix
}
