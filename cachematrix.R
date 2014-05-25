## Filename:  cachematrix.R
##
## Author:    Jennting Timothy Hsu
## Date:      May 24, 2014
##
## Purpose:  
##    This R script is composed for the Assignment 3 of the course 'R Programming' 
##    offered by Johns Hopkins University through coursera.org
##
##    This R script contains following two functions:
##      1. makeCacheMatrix
##      2. cacheSolve
##
##    The function 'makeCacheMatrix' will
##      1. Cache input matrix and correponding inverse matrix
##      2. Display input matrix and corresponding inverse matrix
##
##    The function 'cacheSolve' will calculate, cache and return inverse of input matrix.
##
##    If the inverse has already been calculated (and the input matrix has not changed), 
##    then the function 'cacheSolve' will retrieve the inverse form the cache.
##
## Usage Example:
## > source("cachematrix.R")
## > x = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## > x$get()
## > cacheSolve(x)
## > x$get_invMatrix()
## > cacheSolve(x)
## 
## > x$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
## > cacheSolve(x)
## > x$get()
## > x$get_invMatrix()
## > cacheSolve(x)


makeCacheMatrix <- function(x = matrix()) {
  ##  Purpose:        Cache input matrix and correponding inverse matrix
  ##                  Display input matrix and corresponding inverse matrix
  ##
  ##  'makeCacheMatrix' contains following 4 methods:
  ### set(matrix):                Re-input (can cache) a new matrix 
  ###                             and erase cached inverse matrix
  ### get():                      Display input matrix
  ### set_invMatrix(matrix_inv):  Cache calculated inverse matrix
  ### get_invMatrix():            Return matrix inverse
  
  invMatrix <- NULL   ## Erase existing inverse matrix
  
  ## Re-input (can cache) a new matrix and erase cached inverse matrix
  set <- function(y){
    message("Re-input a new matrix and erase cached inverse matrix")
    x <<- y             ## cache input matrix
    invMatrix <<- NULL  ## Erase cached inverse matrix
  }
  
  ## Display input matrix
  get <- function(){
    x
  }
  
  ## Cache calculated inverse matrix
  set_invMatrix <- function(z){
    message("Cache calculated inverse matrix")
    invMatrix <<- z
  }
  
  ## Display inverse matrix
  get_invMatrix <- function(){
    invMatrix
  }
  
  ## Function return
  list(set = set, get = get, 
       set_invMatrix = set_invMatrix, 
       get_invMatrix = get_invMatrix)
}


cacheSolve <- function(x, ...) {
  ## Purpose: Calculate, cache and return inverse of input matrix
  ## Usage:   Work with function 'makeCacheMatrix'
  ## Example: amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
  ##          cacheSolve(amatrix)
  
  ## Check existing (cached) inverse matrix
  ## Use cached inverse matrix if the existing inverse matrix is not empty
  invMatrix <- x$get_invMatrix()
  if(!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)         ## End of function 'cacheSolve'
  }
  
  ## Calculate inverse of input matrix if cached inverse matrix is empty
  xx <- x$get()               ## Extract input matrix from input object
  invMatrix <- solve(xx)      ## Calculate inverse of input matrix
  x$set_invMatrix(invMatrix)  ## Cache calculated inverse matrix

  ## Function return
  return(invMatrix)
}
