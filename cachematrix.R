## This file includes the functions that utilise and demonstrate the R concepts of free
## variables and cache.

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ##variable to hold the inverse matrix
    im<-NULL
    
    ##set free variable of matrix 'x' to matrix data and free variable 'im' to Null
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    
    ##define the get function to get the matrix assigned in x
    get <- function() {
        x
    }
    
    ##set the inverse matrix free variable
    setInverse <- function(inverse) {
        im <<- inverse
    }
    ##get the 
    getInverse <- function() {
        im
    }
    
    ##list of subroutines
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getInverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    matrix <- x$get()
    im <- solve(matrix, ...)
    x$setInverse(im)
    im
}
