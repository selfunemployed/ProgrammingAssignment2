## This file contains functions that improve the performance of inverse matrix
## calculations by use of a cache.

## makeCacheMatrix() creates a "matrix" which can store its inverse once it has 
## been calculated. It is implemented as a list with 4 members ($set, $get,
## $setinv, and $getinvm). The built-in solve() function is used to calculate 
## the inverse of the matrix provided. This function accepts a single argument
## "x" which must be an invertible matrix.
makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinvm <- function(solve) invm <<- solve
    getinvm <- function() invm
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
}


## cacheSolve() returns the inverse of a special matrix created by the
## makeCacheMatrix function above. If the inverse of the special matrix has not
## yet been calculated, the calculation will occur and the result will be
## returned. If the inverse has been calculated and stored in the cache, the
## calculation will not execute and the cached value will be returned directly.
## This function accepts an object "x" which must have been created by the 
## makeCacheMatrix function, and any additional parameters are passed to the
## solve() function
cacheSolve <- function(x, ...) {
    invm <- x$getinvm()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinvm(invm)
    invm
}