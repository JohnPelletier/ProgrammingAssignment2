##
##Programming Assignment 2 - John Pelletier
##
#######################################################################
## makeCacheMatrix function
## Creates a special kind of "matrix" that can run these four 
## sub-commands: get, set, getinverse and setinverse
#######################################################################
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}

#######################################################################
##
## cacheSolve function
## Gets a cached value for the inverse of the given matrix if it
## exists and the matrix hasn't changed. Otherwise it calculates it
## and then caches it in the special purpose cacheMatrix created above
#######################################################################
cacheSolve <- function(x, ...) {
  
  ## try to get the cached inverse matrix
  inv <- x$getinverse()
  
  ## if it exists, then return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise, get the cached matrix
  data <- x$get()
  
  ##The instructions say to assume the given matrix is always invertible
  
  ##Invert the matrix  
  inv <- solve(data)    
  
  ##cache the inverse matrix
  x$setinverse(inv)
  
  #return the result
  inv
  
}
