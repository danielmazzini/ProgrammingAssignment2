## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    # holds the cached value or NULL if nothing is cached
    # initially nothing is cached so set it to NULL
    inverse <- NULL
    # store a matrix
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    # returns the stored matrix
    get <- function() return(mtx);
    # cache the given argument 
    setinv <- function(inv) inverse <<- inv;
    # get the cached value
    getinv <- function() return(inverse);
    # return a list. Each named element of the list is a function
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special
## matrix returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}
