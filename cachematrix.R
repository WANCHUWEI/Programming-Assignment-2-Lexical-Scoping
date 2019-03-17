## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        J <- NULL
        ##set the matrix
        set <- function(matrix) {
                M <<- matrix
                J <<- NULL
        }
        ##get the matrix
        get <- function() M
        ##set the inverse of the matrix
        setinverse <- function(inverse) J <<- inverse
        ##get the inverse of the matrix
        getinverse <- function() J
        ## Return a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
##The following function calculates the mean of the special "vector" created with the makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        M <- x$getinverse()
        ##return the inverse
        if (!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        data <- x$get()
        ##Calculate the inverse by using matrix
        M <- solve(data) %*% data
        ## Set the inverse to the object
        x$setinverse(M)
        ## Return the matrix
        M
}
