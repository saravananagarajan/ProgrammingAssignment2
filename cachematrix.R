## makeCacheMatrix function has 4 functions internally. It gets the matrix value and sets it to i. Also setinverse function gets inverse of a matrix and stores it in "i".  
## cacheSolve matrix 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	
	i <- NULL
	
	set <- function(y){
		x <<- y
		i <<- NULL    
	}
	
	get <- function(){
		x
	}
	
	setinverse <- function(inverse){ 
		i <<- inverse    
	}
	
	getinverse <- function(){
		i
	}
	
	list(set = set , get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve matrix functions searches for the availability of the inverse of a matrix in cache, if the cache value "i" is not null , then inverse of a matrix is returned.
## If not, inverse of the matrix is calculated and set to variable "i". 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
		
		if(!is.null(i)){
			message("getting cached matrix inverse data")
			return(i)
		}
		
		data <- x$get()
		i <- solve(data, ...)
		x$setinverse(i)
		i  
}
