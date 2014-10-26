## makeCacheMatrix is a funciton that creates a list of functions: 1) the set
##function to fill in the matrix, 2) the get matrix to get the values of the matrix
##3) the setinverse function that stores the inverse matrix and 4)the getinverse
##function to get the values of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <-function()x
  setinverse <-function(inverse) inv<<-inverse
  getinverse <- function () inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## CacheSolve is a function that returns the inverse matrix of its input.
## If it is calculated already, returns the chached value.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}
