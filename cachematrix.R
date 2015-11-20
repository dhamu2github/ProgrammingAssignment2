# Following two functions creates a special "matrix" object that can cache its 
# inverse of a matrix.In general,Matrix inversion is a costly computation but 
# there are some benfits like caching the inverse of a matrix instead of compute 
# it repeatedly.

# The first function, makeCacheMatrix creates a list containing a function to
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of inverse of the matrix
# 4.get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# In this function first checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. If not, it computes the 
# inverse, sets the value in the cache via setinverse function.
# Note: assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

######################### Sample usage #################################
# Create a matrix x
# > x <- matrix(rnorm(4), nrow = 2)

# Create our special matrix
# > cx <- makeCacheMatrix(x)

# Return the matrix
# > cx$get()

# Return the inverse
# > cacheSolve(cx)

# Call the 2nd time, so return the cached inverse
# > cacheSolve(cx) 

########################################################################                                             

