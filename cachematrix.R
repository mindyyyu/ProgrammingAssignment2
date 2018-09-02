## These two functions calculate the inverse of matrix by caching it in memory rather than repeat
## calculation of inverse. This saves time for the subsequent computation.

## The function makeCacheMatrix() creates an R object that stores a vector and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- matrix()
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse #fetch the m value from cachesolve
        getinverse <- function() m     #update the m value in makecachematrix
        list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## The function cacheSolve() requires arguments from makeCacheMatrix() to retrieve the 
## inverse from the cached value stored in environment of makeCacheMatrix().

cacheSolve <- function(x, ...) {   #argument x in makeCacheMatrix is different from
        m <- x$getinverse()        #argument x in cacheSolve
        if(!is.na(m)) {            #argument x in here can be replaced by any other names
                message("getting cached data")
                return(m)
        }            
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
