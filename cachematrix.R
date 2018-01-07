##                                  Caching the inverse of a matrix - R programming assignment 2

# Matrix inversion is usually a costly computation and there may be some benefit to caching the 
# inverse of a matrix rather than computing it repeatedly

# For reducing the computation stress, we have created a pair of functions which can be used to cache the inverse of a matrix


## The below function will create a matrix object that can cache its inverse.
# argument 
# x: a matrix
# function return : A matrix with functions to get/set its value & get/set its inverse

makeCacheMatrix <- function(x = matrix()) {
  # caching inverse of the matrix , assigning inverse as null
  
  inv <- NULL
  
  # setter for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # getter for the matrix
  get <- function() x
  
  # creating both setter and getter for the inverse of the matrix
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  
  
 # function return
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## The below function will calculate the inverse of the matrix object created by the "makeCacheMatrix" function defined above. 
# It will retrieve the inverse from the cache if its already has been calculated, otherwise it will calculate the inverse
# argument : x: a matrix and extra arguments (.....)
# function return : inverse of the matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # storing the value of matrix inverse in variable "inv"
  inv <- x$getInv()
  
  # if condition to check if inverse has been already computed, if computed then return cached inverse

  if (!is.null(inv)) {
    message("Getting cached inverse data")
    return(inv)
  }
  
  # if inverse not computed, then calculated 
   mat <- x$get()
 
  inv <- solve(mat, ...)
  
  # inverse cached
  x$setInv(inv)
  
  # return inverse of matrix
  return(inv)
  
}
