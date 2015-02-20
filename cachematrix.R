## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
        m_inverse <- NULL
        setMatrix  <- function (y) {
                x <<- y                 # Setting the matrix x to new matrix y
                m_inverse <<- solve(x)    # Calculating the inverse of matrix and caching it in variable inverse
        }
        getMatrix <- function () x  #Return the matrix
        setInverse <- function(inverse) m_inverse <- inverse
        getInverse <- function() m_inverse
        list( setMatix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse )        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        original_matrix <- x$getMatrix
        inverse <- x$getInverse
        if ( original_matrix == x && !is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        inverse <- solve(original_matrix, ...)
        x$setInverse(inverse)
        inverse
        
}
