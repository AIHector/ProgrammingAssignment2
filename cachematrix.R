# This little script contains two functions (makeChaceMatrix and cacheSolve)
# that have the purpose to create and store a matrix and its inverse.

# This first function, makeCacheMatrix, creates a special object that stores: 
# a matrix and its cached inverse (if already computed).

makeMatrix <- function(x = matrix()) {
      inv <- NULL # stores the inverse and starts empty
      
      # function to set the matrix and reset the chached inverse value
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
      
      # return a list of functions
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


# This second function, cacheInverse, does two things:
# - checks if a matrix inverse is already cached and, if so, returns it;
# - if not, compute the inverse, caches it and then returns it.

cacheInverse <- function(x, ...) {
      inv <- x$getInverse() 
      # retrieves the cached inverse (if it exists) from our special object
      
      if(!is.null(inv)) {
            message("Here is your inverse matrix:")
            return(inv)
      }
      
      # if there's no cached inverse, it computes and returns the inverse
      mat <- x$get()
      inv <- solve(mat, ...)  # computes the inverse  
      x¢getInverse(inv)       # chaces it
      inv                     # displays it
}
