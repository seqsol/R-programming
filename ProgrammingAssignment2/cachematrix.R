## The functions compute and cache the inverse matrix of a given matrix 
## so that it will not be computed again when it is needed 

## The function receive a matrix as its input and return a list of 
## four functions that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y) {
        x<<-y
        inv<<-NULL
    }
    get <- function () x
    setinv <- function(inv_m) inv <<- inv_m
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function compute the inverse of the special matrix returned from 
## above function. If the inverse already exists, then the function will just 
## retrive it from the cache 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached inverse matrix")
        return(inv)
    }
    else{
        m <- x$get()
        inv <- solve(m, ...)
        x$setinv(inv)
        inv
    }
}
