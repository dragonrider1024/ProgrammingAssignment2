## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# create a new type "makeCacheMatrix" from the regular matrix x

makeCacheMatrix <- function(x = matrix()) {
	invmatrix <- NULL
	set <- function(y) {
		x <<- y
		invmatrix <<- NULL
	}
	get <- function() x
	setinv <- function(inv) invmatrix <- inv
	getinv <- function() invmatrix
	list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# calculate the inverse of regular matrix from the new type makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached inverse matrix")
		return(inv)
	}
	matrix <- x$get()
	inv <- solve(matrix, ...)
	x$setinv(inv)
	inv
}
