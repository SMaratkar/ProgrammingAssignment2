## This file contains the functions to compute the inverse of the square matrix and cache it.
## Once computed the inverse, it will be cached and will not get computed again.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
 
  InvMat <- NULL
  
  set <- function(y) 
  {
    Mat <<- y
    InvMat <<- NULL
  }
  
  get <- function() x
  setInvMat <- function(Mat) InvMat <<- Mat
  getInvMat <- function() InvMat
  
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InvMat <- x$getInvMat()
  
  if(!is.null(InvMat)) 
  {
    message("getting cached matrix")
    return(InvMat)
  }
  
  Mat <- x$get()
  InvMat <- tryCatch(solve(Mat), error = function(e) ("Failed to compute inverse of the matrix."))
  x$setInvMat(InvMat)
  InvMat
}
