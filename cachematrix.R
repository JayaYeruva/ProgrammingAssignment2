## Input : matrix, that is invertible
## Output : list of functions that opearte on the given matrix

## We need to collect the output of below function into an object.
## That object is used to get Inverse of the matrix.
##
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Input : list of functions returned by makeCacheMatrix()
## Output : Inverse of the matrix, which was input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x$get()'
	m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
