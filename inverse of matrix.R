## This function is used to creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmatrix <- function(y){
        x <<- y
        m <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This fuction is used to compute the inverse of 
## the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    datamatrix <- x$getmatrix()
    m <- solve(datamatrix, ...)
    x$setinverse(m)
    m
}
