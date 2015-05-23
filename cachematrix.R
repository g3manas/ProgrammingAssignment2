## This program is to calculate the inverse of a non-singular matrix.
## makeCacheMatrix sets an cached variable which stores 
## inverse of a matrix and cacheSolve calculates the inverse 
## matrix if the cached variable is null


## The function makeCacheMatrix creates a matrix object
## that can cache its inverse. Four functions are defined
## set, get, setInverse and getInverse.
## It returns a list with all the functions
makeCacheMatrix <- function(x = matrix()) {
  im<-NULL
  set<-function(y){
    x<<-y
    im<<-NULL
  }
  
  get<-function()x
  
  setInverse<-function(mInverse)im<<-mInverse
  
  getInverse<-function()im
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve takes an object and returns the inverse of a non-singular
## matrix. If the inverse matrix is cached then it will return the value 
## if not the function calculates inverse using solve()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im<-x$getInverse()
  if(!is.null(im)){
    return(im)
  }
  im<-solve(x$get())
  x$setInverse(im)
  
  im
}
