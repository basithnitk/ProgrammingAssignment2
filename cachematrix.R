## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function takes an atomic matrix and generates a special matrix with a list of functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set = function(y){
    x<<-y
    m<<-NULL
  }
  get = function() x
  setinv = function(inv) m<<-inv
  getinv = function() m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
## The cacheSolve function takes the list ouput from the makeCacheMatrix function and returns the inverse of the matrix in it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix,...)
  x$setinv(m)
  m
}
