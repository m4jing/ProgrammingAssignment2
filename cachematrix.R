## This function creates a special "matrix" object that can cache its inverse.
## function name: makeCacheMatrix
## arguments: x, defaults to a matrix by using matrix()

makeCacheMatrix <- function(x = matrix()) {
    in <- NULL
    set <- function(y) {
        x <<- y
        in <- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    matrix(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## function name: cacheSolve
## arguments: x, a *special* matrix created by function *makeCacheMatrix*
##            ..., extra arguments passed in

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse of the matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
