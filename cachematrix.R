## Defines functions "makeCacheMatrix" and "cacheSolve" which create a special  
## matrix object (a list containing get/set methods) and solve for its inverse
## in a smart fashion by caching the result

## Takes a matrix as input and returns a list of get/set functions for 
## manipulating the matrix and its inverse. Assumes a square, invertible matrix.
## WARNING: No input validation of the matrix.

makeCacheMatrix <- function(x = matrix()) {     #input square invertible matrix
        inv <- NULL                             #initialize inverse to NULL
        set <- function(y) {                    #define matrix "set" function
                x <<- y
                inv <<- NULL
        }
        get <- function() x                     #define matrix "get" function                      
        setinv <- function(inverse) inv <<- inverse #define "set" inverse function
        getinv <- function() inv                #define "get" inverse function
        list(set = set, get = get,              #return list of functions
             setinv = setinv,
             getinv = getinv)
}


## Matrix inverse solving function which caches result. First argument must be
## a list created by makeCacheMatrix.

cacheSolve <- function(x, ...) {                #input list from makeCacheMatrix
        inv <- x$getinv()                       #retrieve inverse
        if(!is.null(inv)) {                     #check if cache exists
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                         #retrieve matrix
        inv <- solve(data, ...)                 #compute matrix inverse
        x$setinv(inv)                           #cache inverse
        inv                                     #return inverse of matrix
}
