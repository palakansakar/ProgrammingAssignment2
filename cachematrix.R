#makeCacheMatrix is a function which
#sets the value of the matrix
#gets the value of the matrix
#sets the value of inverse of matrix
#gets the value of inverse of matrix
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <-function (y){
    x <<-y
    inv <- NULL
  }
  get <- function()x
  setinverse <- function(inverse)inv <<- inverse
  getinverse <- function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#This function returns inverse of matrix
#It first checks if the inverse is calculated or not
#If so, gets the result and skipi calculation
#If not it computes the inverse & sets the value in the cache via
#setinverse function
cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("gettting cache data")
    return(inv)
  }
  dat <- x$get()
  inv <-solve(dat, ...)
  x$setinverse(inv)
  inv
}
