## The following functions are used to create a special object that stores a 
## numeric matrix and cache the matrix's inverse

## makeCacheMatrix creates a list containing a function to set the value of the
## value of the matrix, get the value of the matrix, set the value of inverse of## the matrix and get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
	x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The next function returns an inverse of special matrix created with the above## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cache data.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}

