## These two functions work together to cache the inverse of
## a matrix, and then return that cached inverse matrix if, 
## when called, that matrix has not changed (as opposed to 
## computing the inverse matrix again.)

## The makeChacheMatrix function contains four methods that
## 1) set a matrix 
## 2) get the matrix
## 3) set the matrix's inverse
## 4) get the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInverseMatrix <- function(inverse) i <<- inverse
      getInverseMatrix <- function() i
      list(set = set, get = get,
           setInverseMatrix = setInverseMatrix,
           getInverseMatrix = getInverseMatrix)
}


## cacheSolve checks to see if a matrix's inverse has been
## cached, and if so, returns that inverse matrix. If not,
## it calculates the inverse.

cacheSolve <- function(x, ...) {
      i <- x$getInverseMatrix()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setInverseMatrix(i)
      i
}
