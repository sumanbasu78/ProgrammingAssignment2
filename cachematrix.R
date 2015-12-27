## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## function to make a list of functions for getting a matrix and saving its inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  
  matinv <- NULL
  
  set<-function(y){
    x<<-y
    matinv<<-NULL
  }
  
  get<-function() x 
  
  setInv<-function(z) matinv<<-z
  
  getInv<-function() matinv
  
  list(set=set, get=get, setInv=setInv,getInv=getInv)
  
  
  

}


## Write a short comment describing this function

## return the inverse of a matrix if saved in cache 
## calculate and return the inverse of matrix if not saved in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  cache<-x$getInv()
  
  if (!is.null(cache)) {
    
    message("getting cached data")
    
    return(cache)
  }
  
  mat <- x$get()
  
  cache <- solve(mat, ...)
  
  x$setInv(cache)
  
  return (cache)
}
