## These functions demonstrate the ability to store data 
## inside lexical scopes in R
## to demonstrate this ability, a function introduced (constructor?)
## named "makeCacheMatrix"; this function creates an object with internal
## storage capability
## Another function named "cacheSolve" demonstates a usage of object above

## this is a "constructor" function, which creates a list with 4 elements:
## two getter/setter pairs for two internal variables
## one of variables is a value (matrix), and another variable
## is a cache for result of comples operation on value (inverse of a matrix)
## the specific operation is not coded in this function
makeCacheMatrix <- function(x = matrix()) {
  
  # initializing inverse cache
  i <- NULL
  
  # setter for a value
  set <- function(y) {
    x <<- y # setting value
    i <<- NULL # discarding previous cached value
  }
  
  # getter for a value
  get <- function() x
  
  # setter for inverse
  setinverse <- function(inverse) i <<- inverse
  
  # getter for inverse
  getinverse <- function() i
  
  # composing resulting list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## this function is to demonstate a usage of objects above
## 
cacheSolve <- function(x, ...) {
  
  # taking cached result of matrix inversion from inside x
  i <- x$getinverse()
  
  # if cached data is not null, then it was already calculated
  if(!is.null(i)) {
    
    # and we return it along with message
    message("getting cached data")
    return(i)
  }
  
  # otherwise the result was not calculated yet
  # and we need to do it now
  
  # taking dependency
  data <- x$get()
  
  # calculating complex operation (matrix inverse)
  # I have passed diagonal matrix explicitly to avoid
  # bleach via ellipsis
  i <- solve(data, diag(dim(data)[[1]]), ...)
             
  # remembering calculated value inside an object for future use
  x$setinverse(i)
  
  # returning result
  i
  
}
