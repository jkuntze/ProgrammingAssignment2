## A pair of functions are implemented below to cache the inverse 
## of a matrix.

## makeCacheMatrix creates a special "matrix", which returns a list
## that contains functions to set the matrix, get the matrix, set the 
## inverse of the matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) { 
        
        ### initialize the inverse of the matrix with NULL, since
        ### the inverse is yet to be calculated following the matrix
        ### initialization
        inverse <- NULL
        
        ### Function to set the matrix with new values.
        ### Because a new matrix will be stored, the inverse previously
        ### calculated is no longer valid. Assigning NULL to the inverse 
        ### flags this condition.
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ### Function to return the matrix being evaluated.
        get <- function() x
        
        ### Function to set the value of the inverse of the matrix being
        ### evaluated.
        setinverse <- function(i) inverse <<- i
        
        ### Function to return the inverse of the matrix being evaluated.
        getinverse <- function() inverse
        
        ### A List is returned to provide access to the functions defined 
        ### previously from the parent environment
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with
## the above function.

cacheSolve <- function(x, ...) {
        
        ### initializes the local value of inverse with the value stored
        ### in the special "matrix"
        inverse <- x$getinverse()
        
        ### Check to see if the inverse has already been calculated. If so, 
        ### it returns the inverse from the cache and skips the computation.
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ### Otherwise, it calculates the inverse of the matrix (solve function)
        ### and sets the value of the inverse in the cache via the 
        ### setinverse function.
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinverse(inverse)
        inverse
}
