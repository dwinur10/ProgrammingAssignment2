## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {         # This function creates a special "matrix" object that can cache its inverse
     
     # define the argument with default mode of "matrix"
     Inv <- NULL                                    # initialize Inv as NULL; will hold value of matrix inverse
     set <- function(y) {                           # define the set function to assign new
          x <<- y                                   # value of matrix in parent environment
          Inv <<- NULL                              # if there is a new matrix, reset Inv to NULL
     }
     get <- function() {x}                          # define the get function - returns value of the matrix argument
     setInv <- function(inverse) {Inv <<- inverse}  # assigns value of Inv in parent environment
     getInv <- function() {Inv}                     # gets the value of Inv where called
     list(set = set,
          get = get,
          setInv = setInv,
          getInv = getInv)
}



#Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     Inv <- x$getInv()
     if(!is.null(Inv)) {
          message("getting cached data")
          return(Inv)
     }
     dat <- x$get()
     Inv <- solve(dat, ...)
     x$setInv(Inv)
     Inv
}
