## The two functions create a special object that stores a matrix 
## and caches its inverse matrix.

## The makeCacheMatrix function creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ##set value of matrix
    x <<- y
    m <<- NULL
    }
  get <- function() x  ##get value of matrix
  setinverse <- function(solve) m <<- solve  #set value of inverse matrix
  getinverse <- function() m                 #get value of inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }


## The cacheSolve function:
## 1. Checks to see if the inverse matrix has been cached
## 2. If so, returns the cached inverse matrix
## 3. If not, calculates the inverse matrix and sets the value in the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse() #get value of inverse matrix
  if(!is.null(m)) {   #if value has been cached
    message("getting cached data")
    return(m)         #return cached value of inverse matrix
    }
  data <- x$get()     #if not cached, get the matrix
  m <- solve(data, ...)  #calculate the inverse matrix
  x$setinverse(m)        #set the inverse matrix
  m
  }
