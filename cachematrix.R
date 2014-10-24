## The following two functions create cache "storage" for an inverse of an input matrix
## First the cache storage should be initiated by calling the makeCacheMatrix function
## with the initial matrix, then the cacheSolve to find its inverse matrix
## On the second call of the cacheSolve matrix the message "getting cached data" is displayed 
## before displaying the inverse matrix. And this is repetead on every execution of the 
## cacheSolve until the macaCacheMAtrix is called again to change the value of the matrix

##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It consists of definition of a variable to store the inverse of the matrix and 
## functions to set and read the value of the input matrix and the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	rev <- NULL
      set <- function(y) {
	## assigns the input value to X and empties the storage for the inverse matrix to trigger calculation of the new inverse matrix
      		x <<- y		
      		rev <<- NULL
            }
	get <- function() x  ##returns the value for x
      setrev <- function(reverse) rev <<- reverse  ##saves the calculated inverse matrix in the storage variable
      getrev <- function() rev		## retrives the value of the stored inverse matrix
      list(set = set, get = get,
           setrev = setrev,
           getrev = getrev)
}


##    cacheSolve: This function computes the inverse of the special "matrix" 
##			returned by makeCacheMatrix above. 
##			If the inverse has already been calculated 
##			(and the matrix has not changed), then the cacheSolve 
##			will display the message "getting cached data" and 
##			retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	rev <- x$getrev()
      if(!is.null(rev)) {
		message("getting cached data")
            return(rev)		## Return it from cache
	}
	data <- x$get()
	rev <- solve(data, ...)  ##Calculate the inverse matrix
	x$setrev(rev)		## Store it in cache
	rev			##return the inverse matrix
}
