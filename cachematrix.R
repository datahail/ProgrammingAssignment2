## the pair of funtions 'makeCacheMatrix' and 'cacheSolve' cache 
## the inverse of a matrix. 

## the 'makeCacheMatrix' function creates a special 'matrix',
## more precisely a list containing a function to:
## 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, 
	     getinverse = getinverse)
}

## the following function calculates the inverse the special
## 'matrix' created in the above function. First it checks if the 
## inverse of the matrix has already been calculated. If so, it gets
## the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse using the 'solve' function.
## 
## It is assumed that the matrix supplied is a nonsingular 
## (or invertible) square matrix.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv	  ## Return a matrix that is the inverse of 'x'
} 