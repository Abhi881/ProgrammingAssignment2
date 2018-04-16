## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
## Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {inv <- NULL
set <- function(y){
  x <<- y
  inv <<- NULL
}
get <- function() x
setInverse <- function(solveMatrix) inv <<- solveMatrix
getInverse <- function() inv
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

## Testing our function 
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()         # Returns matrix
my_matrix$getInverse()  # Returns matrix inverse, initially return NULL
cacheSolve(my_matrix) # Computes, caches, and returns new matrix inverse
my_matrix$getInverse()  # Returns matrix inverse after solution
my_matrix$get() %*% my_matrix$getInverse() # returns the identity matrix
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2)) # Modify existing matrix
my_matrix$get()         # Returns matrix
my_matrix$getInverse()  # Returns matrix inverse, initially return NULL
cacheSolve(my_matrix)   # Computes, caches, and returns new matrix inverse
my_matrix$getInverse()  # Returns matrix inverse after solution
my_matrix$get() %*% my_matrix$getInverse() # returns the identity matrix
