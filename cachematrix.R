## Programming Assignmmnet 2 by Adrian Armijos

## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly 

##################################################################

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ##Set enviroment values
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ##Get cached matrix
        get <- function() x
        ##Set cached inverse
        setinverse <- function(inverse) i <<- inverse
        ##Get cached inverse
        getinverse <- function() i
        
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        ##If inverse is already calculated return it
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        ##Calculate inverse, set the result to cache in order to
        ##be used in the next call
        i <- solve(data, ...)
        x$setinverse(i)
        
        i
}
