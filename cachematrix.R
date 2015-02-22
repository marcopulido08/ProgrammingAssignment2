## The following two functions create a special matric that can cache its inverse and find the inverse of a matrix.

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
