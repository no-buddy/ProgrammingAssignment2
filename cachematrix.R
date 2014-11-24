## This source code has two functions. One for defining a vector containing
## functions, and another one for 


# Function makeCacheMatrix creates vector containing function calls
# for matrix object that can cache inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInv <- function(mat) m <<- mat
	getInv <- function() m
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


# Function cacheSolve returns inverse. If it was calculated before
# it returns cached value
cacheSolve <- function(x, ...) {
	m <- x$getInv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInv(m)
	m
}


# This function packs it all together
# usage: solveIt(x)
solveIt <- function(x=matrix()) {
	cacheSolve(makeCacheMatrix(x))
}