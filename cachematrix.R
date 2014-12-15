## Put comments here that give an overall description of what your
## functions do

#Creates a matrix used for the cacheSolve function.
#This stores the matrix using superassignment
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list (set = set, get=get,
              setmatrix = setmatrix,
              getmatrix = getmatrix)
}

#This function returns the inverse matrix of the matrix created using makeCacheMatrix
#If matrix has already been run through this function, it will return cached results
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
