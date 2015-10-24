## This function creates a special "matrix" object, 
## and calcuate the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inverse_x <- NULL
    set  <- function(y)
      {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inverse_x <<- inverse
    get_inverse <- function() inverse_x
    list(Set = Set, get = get , set_inverse = set_inverse, get_inverse = get_inverse)

}


## This function returns inverse of matrix , 
## if inverse is already available in cahce 
## else it will calculate using makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_x <- x$get_inverse()
  if(!is.null(inverse_x))
  {
    message("getting inverse matrix from cache");
    return(inverse_x)
  }
  else
  {
    inverse_x <- solve(x$get())
    x$set_inverse(inverse_x)
    message("calculate inverse matrix");
    retun (inverse_x)
  }
  
}
