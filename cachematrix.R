# This little script contains two functions (makeMatrix and cacheInverse)
# that have the purpose to create and store a matrix and its inverse.

# This first function, makeMatrix, creates a special object that stores: 
# - a matrix
# - its cached inverse (if already computed)

makeMatrix <- function(x = matrix()) {
      inv <- NULL # stores the inverse, starts empty
      
      # function to set the matrix and reset the cached inverse value
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      # function to get the matrix
      get <- function() x
      
      # function to set the inverse
      setInverse <- function(inverse) inv <<- inverse
      
      # function to get the inverse
      getInverse <- function() inv
      
      # return a list of the above four functions
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


# This second function, cacheInverse, does two things:
# - checks if a matrix inverse is already cached and, if so, returns it;
# - if not, computes the inverse, caches it, and then returns it.

cacheInverse <- function(x, ...) {
      # retrieves the cached inverse (if it exists) from our special object
      inv <- x$getInverse() 
      
      if (!is.null(inv)) {
            message("Here is your cached inverse matrix:")
            return(inv)  # return cached inverse
      }
      
      # if there's no cached inverse, it first retrieves the matrix
      mat <- x$get()
      
      # some sanity checks: the input must be a square, invertible matrix
      if (!is.matrix(mat)) stop("Input is not a matrix.")
      if (nrow(mat) != ncol(mat)) stop("Matrix must be square.")
      if (abs(det(mat)) < .Machine$double.eps) stop("Matrix is singular (not invertible).")
      
      # compute the inverse
      inv <- solve(mat, ...)   
      
      # cache the computed inverse
      x$setInverse(inv)        
      
      # announce and return the newly computed inverse
      message("Here is your inverse matrix:")
      inv
}