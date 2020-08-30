## This are functions that try to help and make simple
## the work of Caching the Inverse of a Matrix.

## MakeCacheMatrix function creates a special matrix 
## object than can cache its inverse.

## The function creates a vector containing functions
## to set and get the value of the matrix and to set 
## and get the inverse of that matrix.

MakeCacheMatrix <- function(x = matrix()){
  Inv <- NULL
  ## Set Matrix x:
  Set <- function(y){
    x <<- y
    Inv <<- NULL
  }
  ## Get Matrix x:
  Get <- function(){x}
  ## Set Matrix x Inverse:
  SetInverse <- function(inverse){inv <<- inverse}
  ## Get Matrix x Inverse:
  GetInverse <- function(){inv}
  ## List of Methods:
  list(Set = Set, Get = Get, SetInverse = SetInverse, GetInverse = GetInverse)
}

## CacheSolve function calculates the inverse of the special
## matrix object returned by MakeCacheMatrix function.

## The function calculates the inverse of the MakeCacheMatrix
## checking first if the inverse was calculated and then
## setting and creating the inverse from the cache.

CacheSolve <- function(x,...){
  ## Getting Matrix x Inverse:
    Inv <- x$GetInverse()
  if(!is.null(inv)){
    message("Getting Matrix x Inverse")
    return(Inv)
  }
  ## Setting Matrix x Inverse:
  Mat <- x$Get()
  Inv <- solve(Mat,...)
  x$SetInverse(Inv)
  Inv
}
