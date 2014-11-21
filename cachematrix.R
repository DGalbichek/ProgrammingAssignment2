## The following two functions are my implementations for
## the Inverse Caching Matrix and the related calculation

## This is the matrix with the inverse attached

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function is an updated solve function for matrices
## that store their inverses

cacheSolve <- function(x, ...) {

    i <- x$getinv()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    } else {
        message("calculating")
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}