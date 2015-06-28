## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix: This function creates a special "matrix" object
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    get <- function() {
        x
    }
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getinv <- function() {
        inv
    }
    setinv <- function(y) {
        inv <<- y
    }
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinv()
    if (!is.null(x_inv)) {
        print("this is cached inverse matrix")
        return(x_inv)
    }
    x_mat <- x$get()
    x_inv <- solve(x_mat,...)
    x$setinv(x_inv)
    x_inv
}
