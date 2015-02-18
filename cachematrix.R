## Functions to manage a matrix object and its inverse

## This function creates a matrix object caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize 'i' wich will store the inverse of 'x'
	i <- NULL
  ## We must set to NULL the inverse matix cached when data are changed
  set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## This function returns the caching inverse if it was calculated previously.
## If not, the function will calculate it and it will be cached in the matrix object

cacheSolve <- function(x, ...) {
  ## Recovers cached inverse
	i <- x$getinverse()
  # If it isn't NULL, we can use this cached value
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
  # Otherwise we must calculate and store it
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
