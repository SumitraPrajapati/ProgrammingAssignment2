# This finction is used to create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # Define function to set the value of the matrix. It also clears the od inverse from the cache
     set <- function(y) {  
    x <<- y  # set the value
    i <<- NULL #Clear the cache
  }
    get <- function() x  # Define function to get the value of the matrix
      setinverse <- function(inverse) i <<- inverse # Define function to set the value of the inverse. This is only used by getinverse() when there is no Cache inverse
      getinverse <- function() i # Define function to get the value of the inverse
    list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)  #return the list with above 4 functions
}


## The function below return inverse of the matrix x from makeCacheMatrix above. If the inverse has already been calculated then the cacheslove will retrives the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()  # This fetches the cached value for the inverse
  if (!is.null(i)) { # If the cache was not empty, we can just return it
    message("getting cached data")
    return(i)
  }
  # The cache was empty. We need to calculate it, cache it, and then return it
  data <- x$get() # Get value of matrix
  i <- solve(data, ...) # Calculate inverse
  x$setinverse(i) # Cache the result
  i # Return the inverse
}
       

