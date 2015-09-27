## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix 
# Creates a list of 4 functions to manipulate a matrix and its inverse
#   SET, GET, SETINVERSE, GETINVERSE

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # set and get the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    # set and get the inverse matrix
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    # list containing the 4 functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# cacheSolve 
# Returns the inverse of matrix, after checking cached matrix existence

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    # test existence inverse matrix
    if(!is.null(inv)) {
        message("Cached inverse matrix") 
        return(inv)
    }
    zematrix <- x$get()
    inv <- solve(zematrix)
    x$setinverse(inv)
    inv
}
