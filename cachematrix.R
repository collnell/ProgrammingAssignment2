## caching the inverse of a matrix
## saves time! avoids repeating computations

## first, write a function creates a special "matrix" object that can cache its inverse
##result should return a list of functions that: set matrix, get matrix, set inverse, get inverse
#then put that into cacheSolve ?
##so x needs to be matrix(c(values),nrow=row,ncol=col)
makeCacheMatrix <- function(x = matrix()) {
  inverz <- NULL
  set <-function(y=matrix()) {
    x <<- y ##cache
    inverz<<- NULL
  }
  get = function() x
  setinverz = function(inverse) inverz <<- inverse
  getinverz = function() inverz
  list(set=set, get=get, setinverz=setinverz, getinverz=getinverz)
}


## then a function that computes the inverse of the matrix from above
##if the inverse has been calculated and the matrix is the same, pull inverse from cache
##otherwise compute

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverz<- x$getinverz()
  if(!is.null(inverz)){
    message("getting cached data")
    return(inverz)
  }
  data.matrix<-x$get()
  inverz<-solve(data.matrix, ...)
  x$setinverz(inverz)
  return(inverz)
}


