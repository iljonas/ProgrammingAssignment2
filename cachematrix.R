# Isaac Jonas
# cacheMatrix.R
# Description: These functions create a special list object that stores a matrix as well 
# as caching its inverse

# The makeCacheMatrix function creates a list which serves as a matrix and its inverse,
# based on the matrix passed to it as an argument. This special matrix contains functions 
# to: set the matrix value, get the matrix value, set the inverse value, and get the 
# inverse value
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_matrixinverse <- function(matrixinverse) m <<- matrixinverse
  get_matrixinverse <- function() m
  list(set = set, get = get,
       set_matrixinverse = set_matrixinverse,
       get_matrixinverse = get_matrixinverse)
}

# The function cacheSolve calculates the inverse of the special matrix, created in the 
# makeCacheMatrix function, with the the solve() function. It first checks to see if 
# the inverse has already been calculated, getting the inverse and exiting the function 
# if so. If not, it calcualtes the inverse and sets the value in the cache
cacheSolve <- function(x, ...) {
  m <- x$get_matrixinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_matrixinverse(m)
  m
}