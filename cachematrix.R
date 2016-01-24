# These functions set a matrix, then calcualte its inverse.
# If the inverse is requested and exists in cache from a previous calculation,
# the cache version will be used instead of calculating the inverse again.

# makeCacheMatrix returns a vector of four funcitons. These are used to
# set the initial matrix, retrieve it, set the matrix inverse in cache,
# and retrieve cache version of the matrix

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    
    setMatrix <- function(y) {
        # set the matrix of the parent environment of makeCacheMatrix
        # to the input matrix
        x <<- y
        # Change the matrix inverse to NULL since we have a new matrix
        # This will allow us to detect that we need to calculate a new inverse
        mi <<- NULL   
    }
    
    getMatrix <- function() {
        # Return  x from the parent environment of makeCacheMatrix, which is 
        # the matrix set by a previous use of makeCacheMatrix$setMatrix
        x
    }
    
    setMatrixInverse <- function(matrixInverse) {
        # Take the matrixInverse passed into the function, and set it in the 
        # parent frame's mi matrix to be used for cache
        mi <<- matrixInverse
    }
    
    getMatrixInverse <- function() {
        # Return the matrixInverse from the parent frame's cached mi matrix
        mi
    }
    
    # Return the four functions
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setMatrixInverse = setMatrixInverse, 
         getMatrixInverse = getMatrixInverse)
}




# cacheSolve receives a matrix as input, and returns the matrix inverse.
# If the inverse from this matrix has been calculated previously, use the 
# cached inverse. If not, calculate the inverse and put it in cache.

cacheSolve <- function(x, ...) {
    # Get the current value of the matrix inverse
    micv <- x$getMatrixInverse()
    if(is.null(micv)) {
        # We don't have a cached value, caluclate the inverse and store it
        print("Solving for matrix inverse")
        micv <- solve(x$getMatrix())
        x$setMatrixInverse(micv)
    } else {
        print("Using cached matrix inverse")
    }

    # Return the matrix inverse, which will either be what we got from cache,
    # or what we calcualted just now and put into the cache
    micv
}
