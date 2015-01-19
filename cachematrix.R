## These functions provide an environment where matrix inversion on
## an unchanging matrix is never repeated. Once an inversion is 
## calculated, it is stored in the cache, and the inverted matrix
## is returned in subsequent calls, instead of doing the calculation again

## This is similar to a class in OOP. This function returns pointers
## to functions that create and maintain a cache of inverted matrices
## (a state)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL # if x has changed, then reset inv to NULL
    }
    get <- function() x
    setinverse <- function(invInput) inv <<- invInput
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function calculates the
## matrix inversion. It is also checks if the matrix has
## already been inverted, in which case, the cached value is returned,
## and lots of computing time is saved.

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

## Test these two functions with a few simple cases
testCacheMatrix <- function() {
    mym <- matrix(c(1,0,0,0,1,0,0,0,1), 3,3)
    mycm <- makeCacheMatrix(mym)
    cacheSolve(mycm) # run first time, should calculate inverse
    cacheSolve(mycm) # run 2nd time, should get value from cache
    
    mym2 <- matrix(c(3,1,2,2,0,5,1,2,3), 3, 3)
    mycm2 <- makeCacheMatrix(mym2)
    cacheSolve(mycm2) # run first time, should calculate inverse
    cacheSolve(mycm2) # run 2nd time, should get value from cache
    
    mycm2$set(mym) # set to another matrix
    cacheSolve(mycm2) # check that inverse recalculated
}
