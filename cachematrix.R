## Creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## Returns the inverse of the "matrix" returned by makeCacheMatrix. (The inverse of x)
## Retrieves the inverse from the cache, if it has already been calculated.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
