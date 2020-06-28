## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  #inverse property
    m <- NULL
    
    set <- function(matrix) {
      x <<- matrix
      m <<- NULL
    }
    
    get <- function() x
    
    # set inverse
    setinverse <- function(inverse) m <<- inverse
    
    #getinverse
    getinverse <- function() m
    
    
    # list of methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
  
  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
     #calculate the inverse 
    m <- solve(data) %*% data
  
  x$setinverse(m)
  
  m #return inverse
  
  
}
