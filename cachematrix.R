makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve(x)
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
        m <- x$getinverse()     
        if(!is.null(m)) {
                message("getting cached data")
                return(m)  
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setinverse(m)
        m
}
bmatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
bmatrix$get()         # Returns original matrix
cacheSolve(bmatrix)   # Computes, caches, and returns matrix inverse
bmatrix$getinverse()  # Returns matrix inverse
cacheSolve(bmatrix)   # Returns cached matrix inverse using previously computed matrix inverse