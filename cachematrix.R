##############################################
## Function: makeCacheMatrix
## Create a special matrix object, which is actualy a list containing a function to:
## 1. Set the value of a matrix
## 2. Return the value of the matrix
## 3. Set the value of an inverted matrix
## 4. Return the value of the inverted matrix
##
## Author: Anthony Freeman
## 
## Version	Date		Author	Description
## 0.0.1	21/10/2015	AF			Initial Version
## 0.0.2	21/10/2015	AF			Corrected some commenting errors
##
#############################################

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  # declare the set function
  set <- function(y) {
    x <<- y      # assign at environment level
    m <<- NULL   # assign at environment level
  }
  #declare the function get, returning matrix x
  get <- function() x
  setinverse <- function(inv_vect) m <<- inv_vect  # assign the inverted matrix to m
  getinverse <- function() m    # return inverted matrix
  
  # create and return the list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##############################################
## Function: cacheSolve
## Calculate the inverse of a matrix. If already calculated return it. Otherwise calculate it, cache it and return it
## Parameter: Invertible matrix (square, non singular)
## Author: Anthony Freeman
## 
## Version	Date		Author		Description
## 0.0.1	21/10/2015	AF			Initial Version
##
##
#############################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #call getinverse function from passed in list 
  m <- x$getinverse()
  #if inverse of this matrix object has already been calculated, return it
  if(!is.null(m)) {
    message("getting cached inverse vector")
    return(m)
  }
  #otherwise calulate it, cache it, then return it
  data <- x$get()   # get the matrix from the passed list x
  m <- solve(data, ...)  #invert the matrix
  x$setinverse(m)   # pass the inverted matrix to be cached
  m # return inverse matrix
}
