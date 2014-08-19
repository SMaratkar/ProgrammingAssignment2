## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 
 InvMat <- NULL
 
 set <- function(y) {
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  InvMat <- x$getInvMat()
  
  if(!is.null(InvMat)) {
    message("getting cached data")
    return(InvMat)
  }
  
  Mat <- x$get()
  InvMat <- solve(Mat)
  x$setInvMat(InvMat)
  
  InvMat 
  
}
