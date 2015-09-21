## The following functions "makeCacheMatrix" & "cacheSolve" is designed to compute the 
## inverse of a square matrix. First it checks whether inverse of the 
## given matrix has already been calculated, if so, function will return 
## the result saved in cache otherwise it will calculate the inverse of 
## the matrix and store the result in cache for later use. Purpose of 
## this fuction is not to call solve() function repeatedly if inverse 
## of the same matrix need to be calculte again.   


## Write a short comment describing this function
#  makeCacheMatrix(matrix) creates a list containing a function to:
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of inverse of the matrix
#   4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
#  When called, it checks if the inverse of the matrix is already calculated,
#  if so, it returns the calculated result saved in cache and skips the rest, 
#  otherwise, it calls the solve(matrix) function and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
