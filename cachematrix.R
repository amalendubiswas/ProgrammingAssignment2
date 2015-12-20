## Caching the Inverse of a Matrix.

## This function creates a special "matrix" object that can cache its inverse.
## This function creates inverseMatrix as an empty or NULL matrix, and also
## creates its four children functions such as set, get, setinverse, and
## getinverse, and returns them as a callable list of functions.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Create inverseMatrix as an empty or NULL matrix.
        inverseMatrix <- NULL
        
        ## Set the value of the matrix.
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        ## Get the value of the matrix.
        get <- function() x
        
        ## Set the value of the inverse matrix.
        setinverse <- function(solve) inverseMatrix <<- solve
        
        ## Get the value of the inverse matrix.
        getinverse <- function() inverseMatrix
        
        ## Return the four children functions as a callable list of functions.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by the
## makeCacheMatrix above. If the inverse of the matrix has already been
## calculated (and the matrix has not changed), then the cacheSolve should
## retrieve the inverse of the matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
        ## On the very first call of x$getinverse() function, the getinverse
        ## finds that inverseMatrix is stored in makeCacheMatrix's environment
        ## and finds it is empty. Thus, it calls setinverse function from within
        ## the get$inverse and performs the calculation to inverse the matrix
        ## and saves the result in inverseMatrix and returns inverseMatrix.
        ## The next time when x$getinverse() is called, inverseMatrix will no
        ## longer be empty and it will return inverseMatrix. This way the CPU
        ## compute resources will be saved.
	
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
