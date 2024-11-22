## Put comments here that give an overall description of what your
## functions do


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL  # Initialize the cached inverse as NULL
  
  # Setter function for the matrix
  setMatrix <- function(newX) {
    x <<- newX        # Set the matrix
    cachedInv <<- NULL  # Reset the cached inverse
  }
  
  # Getter function for the matrix
  getMatrix <- function() x
  
  # Setter function for the inverse
  setInverse <- function(inv) cachedInv <<- inv
  
  # Getter function for the inverse
  getInverse <- function() cachedInv
  
  # Return a list of the functions
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" object.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve the cached inverse if it exists
  
  # If the cached inverse is not NULL, return it
  if (!is.null(inv)) {
    message("Using cached inverse.")
    return(inv)
  }
  
  # Otherwise, calculate the inverse, cache it, and return it
  myX <- x$getMatrix()   # Get the matrix
  inv <- solve(myX, ...) # Compute the inverse
  x$setInverse(inv)      # Cache the inverse
  inv                    # Return the inverse
}
