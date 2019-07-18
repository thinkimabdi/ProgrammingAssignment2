## function to cache the Inverse of a Matrix, to avoid costly computation if we can avoid it

## creates a cache matrix object, which methods to help set, get the matrix and its corresponding inverses

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x<<- y
    i<<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Finds the inverse of  a matrix and saves it to memory
## if inverse already exists, function reads from memory, and does not compute inverse from scratch

cacheSolve <- function(x, ...) {
  cache<-x$getinverse()
  if (!is.null(cache)){
    message("Getting Cached Data")
    return(cache)
  }
  data <- x$get()
  inverseM <- solve(data)
  x$setinverse(inverseM)
  inverseM
}
