## The makeCacheMatrix function creates a special 'matrix' object that can cache 
## its inverse. The cacheSolve computes the inverse of the special 'matrix' 
## returned by makeCacheMatrix. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.


## makeCacheMatrix function takes a square matrix and returns a list of 
## functions for getting and setting the inverse of the square matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    inv = NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<-inverse
    
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve takes the list of functions as its argument and caculates or 
## gets the inverse of a square matrix from cache.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cache data")
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setinverse(inv)
    
    inv
}
