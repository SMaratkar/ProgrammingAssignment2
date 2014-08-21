## This file contains the functions to compute the inverse of the square matrix and cache it.
## Once computed the inverse, it will be cached and will not get computed again.

## This function is to make cache for inverse of the matrix.
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


## Function to compute the inverse of the matrix (if not computed)
## and then cache it. If inverse of the matrix is already cached then return the
## same.
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
