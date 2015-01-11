# makeCacheMatrix and cacheSolve allow to calculate the inverse of a matrix and
# cache the results so they can be retrieved quickly on subsequent accesses.
# A matrix needs to be converted first with makeCacheMatrix to make the caching
# available. Afterwards, cacheSolve can be called on the converted matrix
# repeatedly.
# Example:
# M <- matrix(c(1, 2, 3, 4), ncol=2)
# cacheM <- makeCacheMatrix(M)
# cacheSolve(cacheM)  # calculates inverse
# cacheSolve(cacheM)  # returns cached inverse

# makeCacheMatrix takes a matrix and turns it into a cacheable matrix
# cacheable matrices support the operations set, get, setinverse and getinverse
# once either the matrix or its inverse are set with the set operations, they
# can be retrieved through the respective get operations
makeCacheMatrix <- function(x = matrix()) {
    # init the inverse to NULL, to mark that it has not been set yet
    inv <- NULL
    # set the matrix operation. sets the matrix and resets its inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get the matrix
    get <- function() x
    # set the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    # get the inverse of the matrix
    getinverse <- function() inv
    # define and return the access to the operations
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve calculates the inverse of matrix 'x'.
# For an inverse to exist, the matrix has to be square and invertible.
# Matrix 'x' needs to be of type 'makeCacheMatrix', since it will try to
# retrieve the inverse from cache first and only calculate it if it has not
# been cached yet.
cacheSolve <- function(x, ...) {
    # try to get the inverse from cache
    inv <- x$getinverse()
    # if it exists, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if not, calculate and cache it
    data <- x$get()  # get original matrix
    inv <- solve(data, ...)  # calculate inverse
    x$setinverse(inv)  # cache inverse
    inv  # return inverse
}
