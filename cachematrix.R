## Put comments here that give an overall description of what your
## functions do
## The cacheSolve is used to solve the inverse of a matrix
## using a special for of the matrix called a CacheMatrix
## that saves the inverse of the matrix if previously solved
## thus caching the data and reducing the need to 
## solve for the inverse each time

## Write a short comment describing this function
## The makeCacheMatrix is used to store information about
## the matrix x, and the inverse of x
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(m_inv) inv <<- m_inv
  getinverse <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## The cacheSolve function takes a variable x of type makeCacheMatrix
## and tries to get its inverse
## If the data for the inverse is already cached, it is used
## otherwise it evaluates the inverse and saves it in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  result <- x$getinverse()
  if (!is.null(result)){
    return(result)
  }
  m_data <- x$get()
  result <- solve(m_data)
  x$setinverse(result)
  result
}
