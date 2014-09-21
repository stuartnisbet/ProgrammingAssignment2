##
## This file contains two functions, makeCacheMatrix() which creates a special "matrix" object that 
## can cache the inverse of the original matrix, and cacheSolve() which returns the inverse of the
## orginal matrix. If the inverse has not been calcuated, cacheSolve() does this and caches the 
## result.  Subsequent calls to cacheSolve() just returns the cached value.
##
##
## Create a special matrix object, implemented as a list, that will return the inverse of the 
## original matrix.  The first call generates the inverse and caches it.  Subequent calls return
## pre-computed and cached value.
##
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse matrix to NULL.  This will be checked upon cacheSolve() to determine
    # whether the inverse needs to be calculated, or whether it can be retrieved from cache
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() invx
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

##
## Calculate the inverse of the input matrix and cache the results.  On subsequent calls, simply
## simply return the previously calculated and cached value.
##
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'.  If it's not been calculated prior, we need
    # to do that.  If it has been calculated before, just return the cached value.
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("Getting cached inverse matrix")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setInv(inv)
    inv
}
