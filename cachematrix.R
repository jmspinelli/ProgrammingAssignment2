## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  inversematrix <- function(solve) m<<- solve
  getmatrix <- function() m
  list(set=set, get=get, inversematrix=inversematrix, getmatrix=getmatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("Getting Cached Inverse Matrix")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$inversematrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

