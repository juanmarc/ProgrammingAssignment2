## This file contains 2 functions:
##   makeCacheMatrix--initializes object that stores functions for computing/caching a matrix inverse
##   cacheSolve--determines if a cache exists and, if so, returns the cached, inverted matrix;
##               otherwise it instantiates a new object and computes/caches/returns the inverted matrix

makeCacheMatrix <- function(x = matrix()){
  ## This function initializes the object for the input matrix and creates functions that can be
  ## used to calculate/cache the matrix inverse, and retrieve the cached matrix inverse.  
  ## The initialized object contains functions to set the input matrix, get back the original input 
  ## matrix, set the cached inverse and get the cached inverse.  This function should be run with
  ## the original invertible matrix as the input argument.
  invMatrix <- NULL
  #set the inverse to null...the setInv function will overwrite this in subsequent calls
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }#set function
  #declare functions to get the original matrix, set/compute the inverse, and retrieve the cached inverse
  #the inverse will be cached such that it only needs to be computed during first instantiation of the object
  get <- function() x
  setInv <- function(solution) invMatrix <<- solution
  getInv <- function() invMatrix
  #define the various function references
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}#makeCacheMatrix function


cacheSolve <- function(x) {
  ## This function will calculate and store the inverse of the input matrix object during the initial execution.
  ## During susequent executions, the inverse will be retrieved as a cached object.
  ## Before executing this function, the makeCacheMatrix function should be executed first.  The object that
  ## results from the first call to makeCacheMatrix should be used as input to this function.
  ## cacheSolve assumes the input is a valid input object from makeCacheMatrix
  invMatrix <- x$getInv()
  #if the inverserse has already been calculated/cached, return/print the cached inverse
  if (!is.null(x$getInv())) {
    message("getting cached data")
    return(invMatrix)
  }#if (!is.null(invMatrix)) 
  #if the inverse has not already been calculated, retrieve the cached origial matrix and compute its inverse
  #saved the inverse to the object for subsequent calls, and print out the results
  data <- x$get()
  invMatrix <- solve(data)
  x$setInv(invMatrix)
  invMatrix
}#cacheSolve function
