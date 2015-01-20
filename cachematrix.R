## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## but it does not compute it. Rather the cacheSolve function calculates the inverse of the 
## special "matrix" returned by makeCacheMatrix. After calculating the inverse once, the inverse
## is cached and easily retrievable. When the inverse is retrieved from the cache, a message follows.

## This function creates a special "matrix" object, including a location to cache the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}

## This function first checks the existing cache. If it is NOT empty, the cached object
## is returned. If it is empty, the inverse of the matrix is calculated and placed in 
## the existing empty cache.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	## by first checking if it has been calculated
	## and cached. If it has, return cached object. If not,
	## calculate it, cache it, and return it.
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
