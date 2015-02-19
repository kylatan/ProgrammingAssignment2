## The following functions are used to cache the inverse of a matrix.

## ----------------------------------------------------------------------------

## makeCacheMatrix is a function that creates a special "matrix" object
## and can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Set the value of the matrix.
        ## This is useful if user wants to input a new matrix to be used 
        ## in cacheSolve.
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Get the value of the matrix. 
        get <- function() x            # Function returns x
        
        ## Set the value of the matrix's inverse and cache it
        setmatrix <- function(solve) m <<- solve
        
        ## Get the value of the matrix's inverse
        getmatrix <- function() m      # Function returns m
        
        ## Return a list of the functions created above
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
        
}



## ----------------------------------------------------------------------------

## cacheSolve is a function that computes the inverse of the special "matrix"
## returned by the makeCacheMatrix function above. If the inverse has already
## been calculated (and the matrix has not changed), then the cacheSolve should
## retrieve the inverse from the cache. The function returns a matrix that is 
## the inverse of 'x'.

cacheSolve <- function(x, ...) {
        
        ## Get the value of the matrix's inverse
        m <- x$getmatrix()
        
        ## Check if the matrix m is null. 
        ## If the matrix m isn't null, then retrieve and return the cached data.
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        ## The following code gets skipped if cached data exists.
        
        ## Obtain the matrix of which an inverse is to be found
        data <- x$get()
        
        ## Determine the inverse of the matrix
        m <- solve(data, ...)
        
        ## Cache the matrix solution in case cacheSolve is called again
        x$setmatrix(m)
        
        ## Return the inverse of the matrix
        m
}