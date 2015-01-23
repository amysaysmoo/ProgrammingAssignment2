## Two functions that work together to calculate the inverse of a square matrix, 
## save it to memory and recall the cached version if recalculation is called.

## First function saves an input matrix and the necessary functions to set and recall output after some manipulations to said matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL 	# clear value
	set <- function(y) {
    	x <<- y
    	inv <<- NULL
 	 }
	get <- function() x # returns input matrix by default
	setinv <- function(invert) inv <<- invert # takes output of inversion calculation 'invert' and assigns it to variable 'inv'
	getinv <- function() inv 	# this will store the cached calculation after first run. returns 'inv' by default
	list(set = set, get = get,
     	setinv = setinv,
    	getinv = getinv) # creates a list that holds the various functions above

}

## Second function takes the first function as an input and checks to see if the inverse is already saved. If so, it returns saved version.
## Otherwise, it calculates the inverse of the matrix from the first function, returns it and saves it to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() 	
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
