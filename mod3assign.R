## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix is set
  }
  
  get <- function() x  # Return the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Set the inverse
  
  getInverse <- function() inv  # Return the inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}





## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)
  
  inv
}

