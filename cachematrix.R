## R Programming - Assignment 2: Caching the Inverse of a Matrix

## The function "makeCacheMatrix" creates a matrix object to cache the inverse of
## a matrix. The function "cacheSolve" either computes the inverse of the matrix 
## object provided by the previous function, if it hasn't already been computed. If it
## has already been calculated, the inverse recalled from the cache.

## The function "makeCacheMatrix" provides a list of four functions enabling the
## user to cache the inverse of a matrix:
## 1) The "set" function allows to substitute the matrix x in the main function
##    with y. In addition, the inverse i is set to null because input data changed
##    and it has to be recalculated.
## 2) The "get" function returns the matrix x which is stored in the main function.
## 3) The "setinverse" function stores the inverse of the input matrix into the 
##    matrix i.
## 4) The "getinverse" function returns the inverse i.

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

## The function "cacheSolve" takes the object where "makeCacheMatrix" is stored
## as an argument. It recalls the inverse with getinverse, if previously stored.
## If the inverse doesn't exist, data is retrieved with get and the inverse is
## calculated. The inverse is subsequently cached using setinverse.

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
