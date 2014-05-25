## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

## makeCacheMatrix: return a list of functions to:
# 1) Set value of matrix
# 2) Get value of matrix
# 3) Set value of matrix inverse
# 4) Get value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  # inv will store cached matrix inverse
  inv <- NULL
  
  # Set for matrix
  set <- function(y){
    x <<- y
    inv <- NULL
  } 
  
  # Get for matrix
  get <- function() x
  
  # Set for matrix inverse
  setinv <- function(inverse) inv <<- inverse
  
  # Get for matrix inverse
  getinv <- function() inv
  
  # Return Set, Get, Set Inv and Get Inv functions
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse is already calculated (and the matrix has not changed), then 
## the cachesolve shall retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  ## If inverse is already calculated, then return it
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # If inverse is not calculated, then calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache inverse
  x$setinv(inv)
  
  # Return inverse
  inv
}
