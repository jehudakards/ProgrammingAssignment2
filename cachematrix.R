## The function that we are creating will create a matrix and cache
## the created functions JUST IN CASE they may be needed later
## which will permit the reduction of the time that would have been consumed
## while recalculating them.

## what would take much time to be calculated will be cached to be called without
## consuming too much time.
## and makeCacheMatrix will do that

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (theMatrix) { ##Here I am setting the value of the matrix using another function
    x <<- theMatrix ## the <<- will be able to modify variable on different levels
    inv <<- NULL
  }
  get <- function() {x} ##for getting theMatrix
  setInverse <- function (inverse)
    inv <<- inverse ##Here the value of the inverse is set
  getInverse <- function () {inv} ## This gets the value of the inverse
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}



## This one computes the inverse of the matrix we created in teh MakeCacheMatrix 

cacheSolve <- function(x, ...) {
  ## This returns a matrix that is the inverse of 'x'
  inv <-x$getInverse ()
  if (!is.null(inv)) { ##if inv is not null
    message ("Wait, we are receiving your cached data")
    return(inv)
  }
  theData <- x$get()
  inv <- solve(theData, ...)
  
  ## SOLVE will compute the inverse
  x$setInverse(inv)
  inv
  
}