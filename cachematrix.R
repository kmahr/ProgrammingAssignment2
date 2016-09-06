## The functions below are able to cache potentially time-consuming computations.
## This is especially useful for computations involving large matrices.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## 1. Set the value of the matrix
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    ## 2. Get the value of the matrix
    get <- function() x
    ## 3. Set the value of the inverse of the matrix
    setinv <- function(inv) invmat <<- inv
    ## 4. Get the value of the inverse of the matrix
    getinv <- function() invmat
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed,) then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinv()
    if(!is.null(invmat)){
        message("getting cached data")
        return(invmat)
    }
    ## If invmat is not calculated, get data from makeCacheMatrix
    data <- x$get()
    invmat <- solve(data)
    x$setinv(invmat)
    invmat
}
