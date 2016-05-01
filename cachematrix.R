## This script should take a matrix and return the inverse matrix using the makeVector
## cacheSolve functions.

## If the inverse matrix has not been calculated, it will be calculated. If the 
## inverse matrix has been calculated, the inverse matrix is returned from
## the cache. 

## makeVector: Create a function that returns a list of 4 functions that can be used with 
## cacheSolve to return the matrix, reset the matrix and inverse matrix data,
## return the inverse matrix, or set the inverse matrix.

makeVector <- function(x = matrix()) {
  
  # Set inverse matrix var to NULL
  inv_m <- NULL
  
  # Reset the current data with new data. 
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  
  # Returns the x argument (should be a matrix).
  get <- function() x
  
  # Set the inv_m var with the inverse matrix.
  set_inverse_matrix <- function(inverse_matrix) inv_m <<- inverse_matrix
  
  # Return the inverse matrix.
  get_inverse_matrix <- function() inv_m
  
  # Organize functions into a list so that they can be called in makeVector.
  list(set = set, get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
}

## cacheSolve: Create a function that takes the makeVector object
## as an input to calculate or return the inverse matrix. 

cacheSolve <- function(x, ...) {
  
  # Get the inverse matrix from makeVector.
  inv_m <- x$get_inverse_matrix()
  
  # If inv_m is not NULL, return the previously calculated inverse matrix (cached). 
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  
  # If inv_m is NULL, then get the data. 
  my_matrix <- x$get()
  
  # Calculate the inverse matrix from my_matrix.
  inv_m <- solve(my_matrix, ...)
  
  # Set the inverse matrix.
  x$set_inverse_matrix(inv_m)
  
  # Return the inverse matrix.
  inv_m
}


## TESTING ##
## Create a 1000 x 1000 matrix
# data <- rnorm(1000^2, 0, 1)
# the_matrix <- matrix(data, 1000, 1000)

# mm <- makeVector(the_matrix)
# lapply(mm, class)  # list of 4, all which are functions. 

## Test to see if the cached inverse matrix is used in the second call to cacheSolve. 
# invMM <- cacheSolve(mm)
# invMM2 <- cacheSolve(mm)
## Result:Passed -- 'getting cached data'

## Sanity check...
# identical(invMM, invMM2)
## Result:Passed -- TRUE