## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        J <- NULL
        set <- function(y) {
                x <<- y
                J <<- NULL
        }
        get <- function() x
        setmean <- function(mean) J <<- mean
        getmean <- function() J
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## Write a short comment describing this function
##The following function calculates the mean of the special "vector" created with the makeCacheMatrix.
##Firstly, checking if the mean has already been calculated.
##If the mean has already been calculated, it should get the mean from the cache and skip the computation.
##If the mean has not been calculated, the mean needs to be calculated from the cache. 

cacheSolve <- function(x, ...) {
        J <- x$getmean()
        if (!is.null(inv)) {
                message("getting cached data")
                return(J)
        }
        data <- x$get()
        J <- mean(data, ...)
        x$setmean(J)
        J
}
