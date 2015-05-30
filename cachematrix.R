## makeCacheMatrix creates an extended matrix object (a list of functions),
## that can cache its inverse. cacheSolve uses that object to
## calculate the inverse and cache it for repeat solves (if the matrix
## did not change).

## makeCacheMatrix takes a matrix as an argument () and
## returns a list of funnctions that set and get the matrix
## and set and get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_mtrx <- NULL
  set <- function(h) {
    if(is.matrix(h)) {
      x <<- h
      inv_mtrx <<- NULL
    } else {
      print(paste("Set function requires a matrix object, not",
                  class(h)))
    }
  }
  get <- function() x
  setinvrs <- function(invrs) inv_mtrx <<- invrs
  getinvrs <- function() inv_mtrx
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}


## cacheSolve takes a special matrix created by the makeCacheMatrix
## function as the first argument (and also accepts any additional arguments
## used by the standard solve function). It first checks whether 
## the inverse matrix hasalready been cached and if not (and the original
## matrix did not change)

cacheSolve <- function(x, ...) {
  inv_mtrx <- x$getinvrs()
  mtrx <- x$get()
  if(!is.null(inv_mtrx)) {
    ## Check if the inverse matrix is of correct dims
    if(all(dim(inv_mtrx) == dim(mtrx))) {
      ident <- round(inv_mtrx %*% mtrx, 8)
      ## Check if the matrix changed (is the above still
      ## identity?)
      if(all(diag(ident) == 1)) {
        message("Retrieving cached inverse matrix")
        return(inv_mtrx)
      }
    }
  }
  inv_mtrx <- solve(mtrx, ...)
  x$setinvrs(inv_mtrx)
  inv_mtrx
}
