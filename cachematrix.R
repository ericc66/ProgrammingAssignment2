## Put comments here that give an overall description of what your
## functions do

# ----------
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

# ----------
# cacheSolve 
# Returns the inverse of matrix, after checking cached matrix existence
cacheSolve <- function(x, ...) {
    # get inverse matrix
    inv <- x$getinverse()
    # test existence cached inverse matrix
    if(!is.null(inv)) {
        message("Cached inverse matrix") 
        return(inv)
    }

    zematrix <- x$get()
    inv <- solve(zematrix)
    x$setinverse(inv)
    inv
}

# Functions examples 
#> x = rbind(c(1,2), c(3,4))
#> m=makeCacheMatrix(x)
#> m$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    3    4
#> m$getinverse()
#NULL
#> cacheSolve(m) > 1st try
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#> cacheSolve(m) > 2nd try
#Cached inverse matrix
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
