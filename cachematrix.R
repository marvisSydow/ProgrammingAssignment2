## makeCasheMatrix is creating a 'matrix' object that stores the matrix and creates an inverse of it and cashes it
## casheSolve is first checking if matrix object does have a cashed inverse or otherwise it's computing it 

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
