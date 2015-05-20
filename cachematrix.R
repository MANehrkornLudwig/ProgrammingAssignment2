# R Programming - Assignment 2: Caching the Inverse of a Matrix

## The function "makeCacheMatrix" creates a matrix object to cache the inverse of
## a matrix. The function "cacheSolve" computes the inverse of the matrix object
## provided by the previous function, if it hasn't already been computed. If it
## has already been calculated, the inverse recalled from the cache.

## The function "makeCacheMatrix" provides four functions.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
};

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
};
