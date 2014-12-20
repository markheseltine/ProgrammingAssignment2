# makeCacheMatrix() takes a matrix and returns a list of
# get and set functions as follows:
#   -   get, set - get and set the value of the stored matrix
#   -   getInverseMatrix, setInverseMatrix - get and set the value of a cached 
#       inverse of the matrix. The inverse value of the matrix defaults to NULL

makeCacheMatrix <- function(x = matrix()) {
    # inverseMatrixCache holds the cached value of the inverse matrix.
    # Its initial value is NULL.
    # This value is replaced with the solved matrix by the setsolve method
    inverseMatrixCache <- NULL
    
    set <- function(newMatrix) {
        # when the val of the matrix is set, 
        # set it value in the enclosing environment and reset the cached value
        # to NULL, also in the enclosing environment
        x <<- newMatrix
        inverseMatrixCache <<- NULL
    }
    get <- function() x
    
    # setInverseMatrix is a function stores the solvedMatrix value in the cache
    # which is held in the enclosing environment
    setInverseMatrix <- function(inverseMatrix) inverseMatrixCache <<- inverseMatrix
    # getInverseMatrix just returns the cached value of the solved matrix
    getInverseMatrix <- function() inverseMatrixCache
    
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


# cacheSolve() takes a matrix x as an argument
# and returns a matrix that is the inverse of x
# It firstly attempts to read the cached value of the inverse
# If no cached value exists then it generates the inverse using the solve method
cacheSolve <- function(x, ...) {
    # Retrieve contents of the cached inverse matrix
    inverseMatrix <- x$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        # if solvedMatrix is not null, that means its reading cached data
        message("getting cached data")
        return(inverseMatrix) # return the cached value
    }
    # (This section is only reached if the cached valued was NULL)
    # Retrieve the (unsolved) matrix
    data <- x$get()
    # Calculate the inverse of the matrix
    inverseMatrix <- solve(data, ...)
    # Store the solved matrix in the cache
    x$setInverseMatrix(inverseMatrix)
    # Return the newly-calculated inverse matrix value
    inverseMatrix
}
