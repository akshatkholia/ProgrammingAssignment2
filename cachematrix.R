## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
			m <- NULL
			set <- function (y) {
				   x <<- y
				   m <<- NULL
			}
			## here we are defining a anonymous function which will give the value of x
			get <- function() x  
			## Allocation of the inverse value to m via <<- operation, which will ensure that the 
			## m is saved in different environment
			setinverse <- function(inverse) m <<- inverse 
			getinverse <- function() m
			list(set = set, get = get, 
			     setinverse = setinverse, 
				 getinverse = getinverse)

}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Also the function has ... argument as well, which accounts for the rest of the arguments of 
## solve function and keep them to be default, unless specified.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
			if (!is.null(m)){
				message("getting cached data")
				return(m)
			}
			data <- x$get()
			m <- solve(data, ...)
			x$setinverse(m)
			m

}
