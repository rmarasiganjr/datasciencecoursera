##Matrix inversion is usually a costly computation and there may be 
##some benefit to caching the inverse of a matrix rather than compute 
##it repeatedly 
##Below are functions that are used to caches an inverse of a matrix.

# This function can cache inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) 
{
	in_inv <- NULL
	set <- function(y) 
	{
		x <<- y
		in_inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list(set = set,get = get,setInv = setInv,getInv = getInv)
}


## Function below computes the inverse of the matrix from the result of the
## "makeCacheMatrix" function 

cacheSolve <- function(x, ...) 
{
	## Return a matrix that is the inverse of 'x'
	in_inv <- x$getInverse()
	if (!is.null(in_inv)) {
			message("getting cached data")
			return(in_inv)
	}
	mat <- x$get()
	in_inv <- solve(mat, ...)
	x$setInverse(in_inv)
	in_inv
}
