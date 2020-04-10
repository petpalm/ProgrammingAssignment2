## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. In this assignment we write a pair of functions that 
## cache the inverse of a matrix. The following two functions will be 
## created to complete the assignment: i. makeCacheMatrix: this function 
## creates a special "matrix" object that can cache its inverse; ii. 
## cacheSolve: this function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 

## The first function, makcacheMatrix, creates a special "matrix", 
## which is really a list containing a function to: set the 
## elements (or input the entries) of the matrix, get the entries
## in the matrix, set the entries in the inverse matrix, and get 
## inverse of the matrix that was entered. 

## The function, ginv(), used to determine the inverse of the 
## matrix is the Moore-Penrose generalized inverse, and it 
## loading the MASS package.  

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(ginv) m <<- ginv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The second function, cacheSolve, determines the inverse
## of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache
## and skips the computation.Otherwise, it calculates the 
## inverse of the data and sets the inverse in the cache via 
## the setinv function.


cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setinv(m)
  m
}
