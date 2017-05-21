## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
