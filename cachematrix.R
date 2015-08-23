## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#   The first function, makeCacheMatrix creates a special "matrix", 
#   which is really a list containing a function to
#   $set: set the value of the vector
#   $get: get the value of the vector
#   $setinv: set the inverse of matrix
#   $getinv: get the inverse of matrix by built-in function solve()
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  # if inverse has been computed, return the computed result
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if inverse has not been computed, calculate the inverse of the matrix
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
