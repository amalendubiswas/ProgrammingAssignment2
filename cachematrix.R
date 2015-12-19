## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the inverse matrix
## Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverseMatrix <<- solve
        getinverse <- function() inverseMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse of the matrix has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse of the matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setinverse(inverseMatrix)
        inverseMatrix
}
