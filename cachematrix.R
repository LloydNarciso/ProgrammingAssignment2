## these functions will enable an inverse of a matrix to be readily accessible
## if it is calculated already; if the inverse isn't calculated, then the function
## cacheSolve will calculate the inverse and store it with the function makeCacheMatrix

## Lloyd Narciso - 11/22/2016

## This function will create a matrix and will store the inverse of the matrix if it was calculated.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x<-y
        inv<-NULL
    }
    get <- function() x
    setInv <- function(i) inv<<-i
    getInv <- function() inv
    invisible(list(set=set,get=get,setInv=setInv,getInv=getInv))
}


## This function will solve for the inverse of the matrix by using the solve function in R; the inverse will then
## be stored in the cache

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
    i<-x$getInv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    i<-solve(x$get(),...)
    x$setInv(i)
    i
}
