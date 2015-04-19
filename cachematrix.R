## Below are two functions that are used to create a special object,
## that stores a matrix and cache's its inversion.

## The first function, makeCacheMatrix(), creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inversed matrix by using the function solve()
## 4. get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
## We assume that X is a square invertible matrix.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The following function, cacheSolve(), calculates an inversed matrix out of a
## special "matrix" created with the above function. However, it first checks
## to see if the inversed matrix has already been calculated. If so,
## it gets the inversed matrix from the cache and skips the computation.
## Otherwise, it calculates the inversed matrix of the data and sets it
## in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

