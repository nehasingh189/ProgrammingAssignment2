## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix  returns a list of functions 
## its function is to store a matrix and a cashed
## version of the inverse of the matrix.
# set      		set the value of a matrix
# get      		get the value of a matrix
# setInverse   	get the cahced value (inverse of the matrix)
# getInverse     	get the cahced value (inverse of the matrix)


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             getinverse = getinverse ,
             setinverse = setinverse )
}

# The following function calculates the inverse of a "special" matrix 
# created with makeCacheMatrix
cacheSolve <- function(x, ...) {
        m <- x$getinverse ()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <-solve(data, ...)
        x$setinverse (m)
        m
}

