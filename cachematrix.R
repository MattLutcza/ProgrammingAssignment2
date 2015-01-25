##
## makeCacheMatrix returns a "special" matrix based on the matrix x passed into the function. The "special" matrix of x really a list with functions to set and get 
## the value of the matrix, as well as functions to set and get the inverse of that matrix. 
##
## cacheSolve returns the inverse of the "special" matrix x passed in. (x has to be the special matrix created by function makeCacheMatrix.) First it tries to see if ## the special matrix passed in already has a non-null inverse, and if so it just returns that inverse (aka gets the inverse from the cache). If the inverse is null, ## then it calculates the inverse with the solve function, stores that inverse in the "special" cache matrix x with the setinverse function, and then returns the 
## inverse.
##

##
## makeCacheMatrix returns a "special" matrix based on the matrix x passed into the function that can also store and retrieve its inverse matrix.
##

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <-function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##
## cacheSolve returns the inverse of the "special" matrix x passed in. (x has to be the special matrix created by function makeCacheMatrix.) It determines if it needs ## to calculate the inverse or if it can just retrieve it from the "special" matrix x.
##

cacheSolve <- function(x, ...) {
	
	i <-x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
        ## Return a matrix that is the inverse of 'x'
}
