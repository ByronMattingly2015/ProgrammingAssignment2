# Coursera: R Programming - Assignment 2
# Title:  Caching the Inverse of a Matrix
# Author: Byron Mattingly
# Date:   28Feb2022

# Library for running Sanity Test
library(testthat)

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse value to NULL
  m <- NULL
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y    # Store called matrix outside of current envt
    m <<- NULL # Reset inverse to null each time the called matrix is changed
  }
  
  # Get called matrix
  get <- function() x
  
  # Set the inverse of the matrix outside of current envt
  setInverse <- function(inverse) {
    m <<- inverse
  }
  
  # Get the inverse of the matrix
  getInverse <- function() {
    m
  }
  
  # Output list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  # Retrieve the inverse from list
  m <- x$getInverse()
  
  # Return a matrix if it is the inverse of x matrix
  if(!is.null(m)) {
    message("retrieving inverse (getting cached data)")
    return(m)
  }
  
  # Otherwise retrieve the called matrix from the list
  data <- x$get()
  
  # Solve the inverse using matrix multiplication
  m <- solve(data, ...)
  
  # And cache it
  x$setInverse(m)
  
  # Return the matrix
  m
}

# Test the functions
x1 <- makeCacheMatrix (matrix(c(1,2,3,4),2,2))
x1$getInverse ()
cacheSolve(x1)
cacheSolve(x1)
x1$set(x1$getInverse())
cacheSolve(x1) # Original matrix?
