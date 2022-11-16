## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL         #inverse matrixes as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  #to get matrix
  setinv <-  function(inver) inv <<- inver
  getinv <- function() {
    inver <- solve(x)
  }
  list(set=set, get=get,setinv= setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data") 
    return(inv) # return inverse value
  }
  data <- x$get()
  inv <- solve(data,...) #calculate the square inverse
  x$setinv(inv)
  inv
}
