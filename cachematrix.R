## The two functions create a special object that stores a matrix 
## and caches its inverse matrix.

## The makeCacheMatrix function creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }


## The cacheSolve function:
## 1. Checks to see if the inverse matrix has been cached
## 2. If so, returns the cached inverse matrix
## 3. If not, calculates the inverse matrix and sets the value in the cache

cacheSolve <- function(x, ...) {
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
