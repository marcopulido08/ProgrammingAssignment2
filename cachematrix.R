## Coursera: R Programming
## Programming Assignment 2
## Marco Pulido

## Solution based on example provided and the link provided in the lecture titled "Demistifying MakeVector Article",
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
## 
## This solution is a variation of one provided by me in 2015.
##
## The program consists of two functions, makeCacheMatrix and cacheSolve.
## The first function, makeCacheMatrix, caches the inverse of its matrix, if it exists.
## The second function, cacheSolve, calculates the inverse of the given matrix if it doesn't exist.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <-function(y){
  x <<- y
  m <<- NULL
}
get <- function() x
setmatrix <- function(solve) m <<- solve
getmatrix <- function() m
list(set=set, get=get,
   setmatrix = setmatrix,
   getmatrix = getmatrix)
}


## This function will retrieve the inverse of a matrix from cache, if it already exists, or it will find it
## using the function solve(), which is an R built-in function which finds the inverse of invertible matrices.

cacheSolve <- function(x = matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m)){
      message("Getting Chached Data of Inverse of Matrix")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
