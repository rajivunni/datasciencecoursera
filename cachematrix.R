## Matrix inversion is usually a costly computation and their may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Here we write write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    ## Computing the inverse of a square matrix can be done with the solve function in R
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    ## set the value 
    ## get the value 
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


##This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    ##If the inverse has already been calculated (and the matrix has not changed), 
    ##then the cacheSolve should retrieve the inverse from the cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ##This function computes the inverse of the special "matrix" 
    ##returned by makeCacheMatrix.R
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}