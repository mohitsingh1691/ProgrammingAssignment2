

#This function is creating a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b)                                    # set the value of matrix 
  {
    a <<- b
    inv <<- NULL
  }
  get <- function() a                                   #get the value of matrix 
  setinverse <- function(inverse) inv <<- inverse       #set value of inverse of matrix
  getinverse <- function() inv                          #get value of inverse of matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function computes the inverse of matrix created by makeCacheMatrix function above.If the inverse is already created, it should 
# retrieve the inverse from cache.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(a, ...) {
  inv <- a$getinverse()
  if(!is.null(inv)) {
    message("cached data is present")
    return(inv)
  }
  c <- a$get()
  inv <- solve(c)
  a$setinverse(inv)
  inv
}

