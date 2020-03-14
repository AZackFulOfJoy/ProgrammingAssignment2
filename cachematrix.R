## As explained by the assignment description, makeCacheMatrix() creates a special matrix object that can...
## ...cache its inverse, while cacheSolve() computes the inverse of the special matrix returned by makeCacheMatrix()...
## ... and if the inverse has already been calculated, then cacheSolve() should retrieve the inverse from the cache...
## ... given that the special matrix has not been changed.
## As a result of lexical scoping, an object variable will contain a complete copy of the environment... 
## ...for makeCacheMatrix() including any objects that are defined in makeCacheMatrix()...
## ... and the object variable will have access to all the functions included in the makeCacheMatrix() function 
## ...and any data in the environment that it references in its functions.

## The first function
## Line 27: Defining the argument input. Take an argument "x" that is a matrix.
## Line 28: Initializing the inverse matrix variable "I" set to NULL so that data can be stored later 
## Line 29: set() takes an argument named "y". It is assumed that "y" is a matrix. 
## Line 30: The operator <<- is used to assign the value on the right side of the operator to an object...
## ...in the parent environment named by the object on the left side.
## Line 31: This line of code is responsible for clearing any value of "I"
## Line 33: Defining the getter for the matrix "x" get(), lexical scoping is applied sine "x" is not defined as argument in get()...
## ...so R will retrieve "x" from the parent environment of makeCacheMatrix()
## Line 34: Defining the setter for the inverse matrix "I" setinverse(), Since "I" is defined...
## ...in the parent environment and we need to access it after setinverse() completes, the code use <<- to assign...
## ...the input assignment to the value of "I" in the parent environment. 
## Line 35: Defining the getter for the inverse matrix "I" getinverse(), and just like the getter for "x"... 
## ...lexical scoping is applied once more to find the correct symbol for "I" to retrieve its value.
## Line 36-38: The makeCacheMatrix() assigns each of its functions as an element within a list()...
## ...and returns it to the parent environment so that the $ operator is functional.

makeCacheMatrix <- function(x = matrix()) { 
  I <- NULL 
  set <- function(y) {
    x <<- y  
    I <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function
## Line 53: Defining the argument input. Takes an argument "x" and an ellipsis (...), that allows the call... 
## ...to pass additional arguments into the function. 
## Line 54: The function attempts to retrieve an inverse matrix from the object passed in as the argument...
## ...it calls the getinverse() function on the input object "x".
## Line 55-57: The function checks to see whether the result is NULL... 
## ...since makeCacheMatrix() sets the cached inverse to NULL whenever a new matrix is set into the object and...
## ...if the value is not equal to NULL, we have a valid cached inverse matrix and can return it to the parent environment.
## Line 59-62: If "if(!is.null(I))" is FALSE (is NULL), then cacheSolve() gets a matrix from...
## ...the input object, calculates solve(), uses the setinverse() function on the input object and then returns...
## ...a value of the inverse to the parent environment.

cacheSolve <- function(x, ...) {
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}
