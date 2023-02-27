## This exercise is about lexical programming. I have designed 2 functions
## that calculate the inverse of a matrix and keep it in memory. When the computation
## is requested afterwards, the inverse is retrieved from memory and a message is issued.
## Otherwise, the inverse is computed.

## The makeCacheMatrix creates a new environment with a list of 4 functions and 2 matrixes
## objects x and m which are passed to the global environemnt with the operator "<<-".
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, 
        get = get,
        setinv = setinv,
        getinv = getinv)
}

## The cacheSolve function calculates  the inverse of a squared matrix.
## This gets input from the result of 
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("The inverse matrix is in cache. No need to compute again.")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

x1<-matrix(1:4,nrow=2,ncol=2)
myMatrix <- makeCacheMatrix(x1)
inverse_matrix<-cacheSolve(myMatrix)
x1
inverse_matrix
x1%*%inverse_matrix
inverse_matrix<-cacheSolve(myMatrix)

