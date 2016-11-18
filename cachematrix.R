## makeCacheMatrix creates a list containing function for
## 1. setting the value of the matrix
## 2. getting the value of the matrix
## 3. setting the value of inverse of the matrix
## 4. getting the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The below function returns the inverse of matrix. It first checks if inverse of matrix 
## has already been computed. If yes, then it gets the result and skips the computation. 
## If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m)
  x$setinverse(inv)
  inv
}
