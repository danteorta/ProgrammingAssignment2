## The function makeCacheMatrix builds a special matrix that can save
## in the cache its own inverse. The function cacheSolve calculates the
## inverse or retrieves it from cache.

## Function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Set the inverse
    invr <- NULL
    # Define the necessary functions
    set <- function(y) {
        x <<- y
        invr <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invr <<- inverse
    getinverse <- function() invr
    # Create the special object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function that gets the inverse of a matrix if it has not
## been calculated or from cache if it has been

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Check if the inverse has already been calculated.
    invX <- x$getinverse()
    # If the inverse already exists, return it.
    if(!is.null(invX)) {
        message("getting cached data")
        return(invX)
    }
    # If the inverse does not exist, get the matrix
    # and calculate the inverse.
    data <- x$get()
    invX <- solve(data, ...)
    # Save the result in cache and return the result
    x$setinverse(invX)
    invX
}
