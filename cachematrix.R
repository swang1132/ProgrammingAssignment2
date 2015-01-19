## The implementation of CacheMatrix Inversion follows the same approach as the example
## shown in the GitHub - Caching the Mean of a Vector. The key operator is <<- which can be used
## to assign a value to an object in an environment that is different from the current environment

## makeCacheMatrix contains a list of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        invCached <- NULL
        set <- function(y) {
                x <<- y
                invCached <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invCached  <<- inverse
        getinverse <- function() invCached 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above
## matrix constructor function. This function takes the following steps to calculate the inverse of
## the matrix : 
##            Step 1. To check whether the inverse has been calculated or not, if yes, return the cached value;
## Otherwise, Step 2. To calculate the inverse, and then cache the value through setinverse function.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        invCached  <- x$getinverse()
        if(!is.null(invCached )) {
                message("getting cached invert data.")
                return(invCached )
        }
        data <- x$get()
        invCached  <- solve(data)
        x$setinverse(invCached)
        invCached
}
