## The pair of functions below cache the inverse of an input matrix. The matrix
## supplied is assumed to be always invertible. 


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated and the matrix hasn't changed, 
## returns the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}