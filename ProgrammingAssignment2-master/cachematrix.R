
## This function is for caching the inverse of a Matrix
## There are benefits to caching the inverse of a Matrix rather than ##compute it repeatedly

## The function makeCacheMatrix below stores a Matrix and caches its
## inverse
## Thus the function creates a special object "Matrix" that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function (y) {
	x <<- y
	inv <<- NULL
	}
	
get <- function () x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}

## The above function computes the inverse of the special "Matrix"
## created by makeCacheMatrix above. such that if the inverse has 
## already been calculated it will skips the computation and retrieve
## the inverse from the cache



## cacheSolve: This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above
## if the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null (inv)) {
        	message("getting cached data")
        	return(inv)
        	}
        	
       matr <- x$get()
       inv <- solve(matr, ...) 
       x$setInverse(inv)
       inv	
        		
}
