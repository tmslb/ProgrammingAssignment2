## The function makecachematrix creates a special matrix object ,which is a list that contains 4 functions: set 
# (to set the value of the matrix), get (to get the value of the matrix), setInv (to set the value of the inverse 
# and getInv (to get the value of the inverse). 


makeCacheMatrix <- function(x = matrix()){
  x.inverse <- NULL                                          # x.inverse stores the result of the inversion
  set <- function(y) {
    x <<- y
    x.inverse <<- NULL
  }
  get <- function() x                                        # Returns the input matrix
  setInverse <- function(inverse) x.inverse <<- inverse      #Sets the inverse matrix
  getInverse <- function() x.inverse                         #Gets the inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function - cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache. If not, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
  m <- x$getInverse() 
  # get the inversed matrix from object x
  
  if(!is.null(m)) { 
    message("getting cached data")   # If the inversion result is there;
    return(m)                        # return the calculated inversion
  }
  data <- x$get()                    # If not, we use x$get to get the matrix object
  m <- solve(data)                   # we use the solve function to compute the inverse
  x$setInverse(m)                    # we then set it to the object
  m                                  # Returns the solved result
}