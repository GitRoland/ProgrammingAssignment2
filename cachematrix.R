## "makeCacheMatrix" creates and returns a special "matrix" object that can 
## cache its inverse. The returned object is a list with the named member functions:
##
##  $set(mat)           - sets the true matrix value.
##  $get()              - returns the true matrix value.
##  $setinverse(inv)    - sets the value of the cached inverse.
##  $getinverse()       - returns the cached inverse of the matrix value.

makeCacheMatrix <- function(mat = matrix()) {
    inverse <- NULL
    
    set <- function(m) {
        mat <<- m
        inverse <<- NULL
    }
    
    get <- function() mat
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## "cacheSolve" accepts a special matrix object created by "makeCacheMatrix"
## and returns the inverse of the true matrix value.
##
## If the inverse has not yet been cached, it is first computed and cached.  Any
## additional arguments passed to cacheSolve are passed along to the solve() 
## call used to caculate the inverse.  NOTE: the extra arguments are ignored in
## the case where the inverse value has already been cached.
##
## Finally, the (cached or calculate) inverse of the matrix is returned.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("getting cached INVERSE data")
        return(inverse)
    }
    
    mat <- x$get()
    message("solving for the matrix value")
    inverse = solve(mat, ...)
    x$setinverse(inverse)
    inverse
}
