## Put comments here that give an overall description of what your
## functions do

## Function to create a matrix to store cached data

makeCacheMatrix <- function(x = matrix()) {  ##set the formal arg x as a matrix
  ## make sure the inverse matrix is empty
  m <- NULL  
	##store the input matrix
	set <- function(y) {  
		x<<- y
		##reset the inverse matrix to null when a new matrix is set
		m<<- NULL   
	}
	##return the input values
	get <- function() x  
	#store the inverse matrix
	setinverse <- function(solve) m <<- solve 
	#retrieve the inverse matrix
	getinverse <-function() m 
	
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## A function to calculate the inverse of a matrix, and then
## store it in a matrix for the future. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     #check to see if we have already stored the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {  
        #the inverse matrix already exists so return this 
        message("getting cached data")
     	  return(m)
      }
        #the inverse matrix doesnt exist, need to calculate it
      #retrieve the input matrix
      data <- x$get() 
      #calculate the inverse matrix
      m <- solve(data, ...)
      #store the inverse matrix 
      x$setinverse(m)
      #return the inverse matrix
      m
}
