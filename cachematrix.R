###############################################
##  My attempt at programming assignment #2  ##
###############################################

##-------------------------------------------------------------------------------------
## To test these functions, source this file and issue the following R code:
## 
## > m1 <- matrix(sample(1:1000,100),10,10)  # 10x10 matrix of random numbers 1-1000
## > m2 <- makeCacheMatrix(m1)   # create the cache functions
## > m3 <- cacheSolve(m2)        # first run, not there, so it calcs and sets value
## > m4 <- cacheSolve(m2)        # second run returns value from cache
##-------------------------------------------------------------------------------------

## Code to create the two functions is below:

##--------------------------------------------------------
## function makeCacheMatrix
##
## This function creates four new functions for caching 
## a matrix and its inverse and retrieving same
##--------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function(){
        x
    }
    setinv <- function(z){
        inv <<- z
    }
    getinv <- function(){
        inv
    }
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinv = setinv, getinv = getinv)
}

##-----------------------------------------------------------------------------
## function cacheSolve
##
## This function returns the inverse of x.  If it is in cache, it returns it.
## If not, it creates it.
##-----------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("using inverse from cache")
        return(inv)
    }
    newmatrix <- x$getmatrix()
    inv <- solve(newmatrix)
    x$setinv(inv)
}

