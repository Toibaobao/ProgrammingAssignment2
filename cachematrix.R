## Coursera assignment 2
## Calculating and cashing the reverse of a matrix.
## Using the package matlib:
##      https://cran.r-project.org/web/packages/matlib/index.html


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
		x <<- y
		m <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(inv) m <<- inv
        getInvMatrix <- function() m
        list(set = set, get = get,
		setInvMatrix = setInvMatrix,
		getInvMatrix = getInvMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInvMatrix()
        if(!is.null(m)) {
		message("Getting cached data...")
		return(m)
        }
        data <- x$get()
        m <- inv(data, ...)
        x$setInvMatrix(m)
        m
}
