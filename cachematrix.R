## The following is a pair of functions that cache and compute the
## inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
     m <- NULL
     set <- function(x) {
         mtx <<- x;
         m <<- NULL;
     }
     get <- function() return(mtx);
     setinv <- function(inv) m <<- inv;
     getinv <- function() return(m);
     return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
##It is assumed the matrix supplied is always invertible

cacheSolve <- function(mtx, ...) {
    m <- mtx$getinv()
    if(!is.null(m)) {
        message("getting cached data...")
        return(m)
    }
    data <- mtx$get()
    m <- solve(data, ...)
    mtx$setinv(m)
    return(m)
}
