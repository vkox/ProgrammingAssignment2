## The two functions below work together to compute the inverse of a given special matrix and cahce the inversed matrix for future retrieval.
## If the inverse of a given matrix is already cached, it retrieves it instead of re-calculating.

# makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# It is really a list of functions which : 
#       1. Set the value of the matrix - set() (or makeCacheMatrix() itself)
#       2. Get the value of the matrix - get()
#       3. Set the value of the inversed matrix - setinverse() 
#       4. Get the value of the inversed matrix - getinverse()
# Except for setinverse(), the other three functions can be called directly. 
# The setinverse() function should not be called directly as it will set the new matrix as inversed without inversing it.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y = matrix()) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(z) m <<- z 
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


# cacheSolve function calculates the inverse of the special "matrix" returned by makeCacheMatrix function. 
# It first checks to see if the inverse of x has already been calculated.
# If yes, it gets the inverse from the cache and skips the computation. 
# Else, it calculates the inverse of the x (matrix) and sets the value of the inversed matrix in the cache via the setinverse function.


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
