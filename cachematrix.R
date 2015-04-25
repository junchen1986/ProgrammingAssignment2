## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    set <- function(y) {
        x <<- y
        mInv <<- NULL
    }
    get <- function() x
    setInv <- function(matInv) mInv <<- matInv
    getInv <- function() mInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mInv <- x$getInv()
    if(!is.null(mInv)) {
        message("getting cached data")
        return(mInv)
    }
    data <- x$get()
    mInv <- solve(data, ...)
    x$setInv(mInv)
    mInv    
}
