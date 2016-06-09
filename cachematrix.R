## The main purpose of this assignment is to write functions that cache the inverse of a matrix.
## An assignment consists of writing 2 different functions: "makeCacheMatrix" and "cacheheSolve".
## Each of these functions will be described below.

## The function given below creates a special matrix object that can cache its inverse.
## The first function, "makeCacheMatrix" creates a special matrix which is a list containing a function to
   ## 1. set the value of the matrix
   ## 2. get the value of the matrix
   ## 3. set the value of inverse of the matrix
   ## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The second function computes the inverse of the special matrix returned by the function above (makeCacheMatrix).
## If the inverse has already been calculated and also the matrix has not changed,
## then the "cacheSolve" will get back the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
