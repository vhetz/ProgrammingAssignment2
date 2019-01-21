
## These functions calculate the inverse of the special "matrix" which is created first by the makeCacheMatrix function. It then checks to see if the inverse has already been calculated, if so it gets the inverse from the cache and skips the compulation. If not it calculates the inverse of the matric and sets the value of the inverse in the cache via setinverse function.
## 

## makeCacheMatrix creates a list containing a function to set/get value of matrix and set/get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y) {
    x <- y
    inv<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv <- inverse
  getinverse<- function() inv
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: The below function returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <= x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


