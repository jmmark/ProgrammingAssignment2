## Two functions--the first creates and returns a list of matrix elements 
## allowing the storage of the inverse of the matrix in the chache, the second
## inverts the matrix only if necessary, returning the cached value if avalable
## and applicable

## creates a matrix which is a list of functions which return the matirx if available, change the
## matrix and clear the caced inverse, return the inverse of the mean, or save the inverse
## in the cache

makeCacheMatrix <- function(x = matrix()) {
    ## inv holds the inverse of the matrix
    inv <- NULL
    ## first define the function to set the special matrix within the overall function
    set <- function(y) {
        x <<- y
        ## if we've reset x, we've chanced it, so need to remove cahced value of inv
        inv <<- NULL
    }
    
    ## method for returning the matrix itself
    get <- function() x
    
    ## method for setting the cached value of the inverse of x
    setInv <- function(inInv) inv <<- inInv
    
    ## method for returning the cached value of the inverse of x
    getInv <- function() inv
    
    ## now return the list of functions
    list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## Returns the inverse of the matrix contained in the above special matrix function
## skips computation and returns cahced value if available
## note--assumes passed matrix is always invertable

cacheSolve <- function(x, ...) {
    ## first return the cached value if available
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("Getting inverse from cache")
        return(inv)
    }
    ## if statement failed, inverse not in cache so calculate
    holdData <- x$get()
    inv <- solve(holdData,...)
    x$setInv(inv)
    inv
    
}
