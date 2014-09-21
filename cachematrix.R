## The functions defined below are designed to handle a special 
## "matrix" object that can cache its inverse. Matrix inversion 
## is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it 
## repeatedly.

## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
	# cache data
	inv <- NULL
	
	# set/get functions
	set <- function(y)
	{
		m <<- y
		inv <<- NULL
	}
	get <- function() m
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	
	# storage list
	list(set = set, get = get, 
		 setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
	# retrieve cache inverse
	inv <- m$getInverse()
	
	# if inverse has already been computed then return inv
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	# if inv is null then compute inverse matrix
	data <- m$get()
	inv <- solve(data, ...)   # we assume data is invertible
	m$setInverse(inv) # set cache value
	
	# return value
	inv
}
