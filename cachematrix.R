
## With this set of functions, one can calculate the inverse of an invertable matrix
## and store it in cache by creating a special "matrix" object using makeCacheMatrix 
## and calling cacheSolve.

## ===============================

##  makeCacheMatrix creates a special "matrix" object,
##  which is really a list containing 4 functions to

##  1.set the value of the matrix
##  2.get the value of the matrix
##  3.set the value of the inverse of the matrix
##  4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()
  set <- function(y) {
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)   
}


##  cacheSolve calculates the inverse of the matrix in 
##  the "matrix" object created with makeCacheMatrix. 
##  However, it first checks to see if the inverse has already been calculated. 
##  If so, it gets the inverse from the cache and skips the computation.
##  Otherwise, it calculates the inverse of the matrix
##  and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.na(m[[1,1]])) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  dimdata <- dim(data)
  dimdata <- dimdata[[1]]
  I <- matrix(nrow=dimdata,ncol=dimdata)
  for (a in 1:dimdata){
    for (b in 1:dimdata) {
      if (a==b) I[[a,b]] <- 1
      else I[[a,b]] <- 0
    }
  }
  m <- solve(data,I)
  x$setinv(m)
  m
}
