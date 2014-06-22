## Functions to cache previously calculated matrix inverse.
## Avoids repeated calculation of inverse of the same matrix
## Always assumes an invertible matrix

## Container function to hold the current matrix and its 
## cached inverse once computed in its environment. 
## It also contains functions to manipulate these variables.

makeCacheMatrix <- function(x = matrix()) {
  # matinv variable contains the cached inverse
  matinv <- NULL
  
  # setmat function set the input vector x to the supplied argument
  # and clears cached inverse as a new matrix has been supplied.
  setmat <- function(inp_mat){
    x <<- inp_mat
    matinv <<- NULL
  }
  
  # getmat function returns the currently help input matrix
  getmat <- function() x
  
  #setcachedinv function sets the cached inverse to the supplied argument
  setcachedinv <- function(inv_mat) matinv <<- inv_mat
  
  # getcachedinv function returns currently cached inverse
  getcachedinv <- function() matinv
  
  #return a list of all functions
  list(setmat = setmat, getmat = getmat,
       setcachedinv = setcachedinv, getcachedinv = getcachedinv)

}


## Function to get the inverse of a matrix that is fed into the makeCacheMatrix
## Depending on whether the inverse for an input matrix is cached, it returns
## either a cached inverse or a newly computed inverse.
## Takes the list returned from makeCacheMatrix as the primary input. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinverse <- x$getcachedinv()
  # Check if cached value of inverse exists and return it else calculate it freshly
  if(!is.null(matinverse)) {
    message("Getting cached inverse...")
    return(matinverse)
  }
  
  # if cached inverse not present get the matrix,
  # solve it and return the calculated inverse and cache it.
  mat <- x$getmat()
  inv <- solve(mat)
  x$setcachedinv(inv)
  inv
}
