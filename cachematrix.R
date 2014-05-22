## Description of functions:

##makeCacheMatrix: 
## creates a special "matrix" object that can cache its inverse.

##cacheSolve: 
## computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cachesolve should retrieve the inverse from the cache.

## Usage Examples
## amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## amatrix$get()         # Returns original matrix
## cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
## amatrix$getinverse()  # Returns matrix inverse
## cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

## amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
## cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
## amatrix$get()         # Returns matrix
## amatrix$getinverse()  # Returns matrix inverse


## cache a matrix 
makeCacheMatrix <- function(x = matrix()) {
  ##initialise a variable to hold the inverse
  m <- NULL
  ## store the matrix and reset the inverse
  set <- function(y) {
            x <<- y
            m <<- NULL
    }
   ##return the matrix
    get <- function() x
   ##set the inverse
    setinverse <- function(solve) m <<- solve
   ## return the inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  ## return a cached inverse matrix if it exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## otherwise find the inverse and return it
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
