## These functions support caching the inverse of a matrix so that the inverse does not need to be (expensively) 
## recalculated each time it is needed


## This makeCacheMatrix function creates a a list of functions to be stored together with a matrix and potentially its its inverse  
## in an environment

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    setM <- function(y){
        x <<- y
        invM <<- NULL
    }
    getM <- function() x
    setinv <- function(invofmat) invM <<- invofMat
    getinv <- function() invM
    
    list( sM=setM, gM=getM, sI=setinv, gI=getinv)
}


## This function obtains the inverse of the matrix. If it has not yet been calculated, it calculcates it and and 
## stores it in the environment set up by makeCacheMatrix; otherwise it retrieves it from the environment.

## x is an matrix object to which an environment object has been associated including the above functions and
## the matrix inverse

## This works for square matrices.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invM <- x$gM
    if (!is.null(invM)){
        message("getting cached matrix")
        return(invM)  
    }
    data <- x$gm()
    invM <- solve(data, ...)
    x$sm(invM)
    invM    
}
