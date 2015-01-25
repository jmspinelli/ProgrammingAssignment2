## Creates a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #Set the value of the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  #Get the value of the matrix
  get <- function() x
  
  #Set the value of the inverse matrix using the Solve function
  inversematrix <- function(solve) m<<- solve
  
  #Get the value of the inverse matrix
  getmatrix <- function() m
  list(set=set, get=get, inversematrix=inversematrix, getmatrix=getmatrix)

}


## Computes the inverse of the matrix returned by makeCacheMatrix.  If the inverse has already been calculated
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()

  # Returns cached matrix if already calculated
  if(!is.null(m)){
    message("Getting Cached Inverse Matrix")
    return(m)
  }
  
  #Computes the inveerse of the matrix if not already cached
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$inversematrix(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}

