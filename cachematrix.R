## Here are two functions to handle the caching of the inverse of a special matrix object.
## R Programming assignment 2.

## makeCacheMatrix handles the creation of the special matrix object.
## It takes a single optional argument, a matrix on which the object is based, which defaults to
## a 1x1 matrix containing NA.
## It returns a list containing the special matrix objects methods 
## set(m) : set or change the matrix to matrix m
## get()  : get the base matrix
## setinverse(inv) : cache the matrix inv as the inverse matrix
## getinverse()    : get the cached matrix, or NULL if none has been set.
## NOTE: it is possible to use setinverse() to set an invalid value for the cached inverse!
## I have only done it like this to match the example for this assignment.
## It would be better to calculate the inverse in setinverse() rather than taking it as a parameter.

makeCacheMatrix <- function(x = matrix()) {
   cached.inverse <- NULL  # We have no cached inverse yet!

   # set the base matrix to parameter y. This is not necessary, but can be done at any time.
   set <- function(y) {
      x <<- y    # save the parameter in the lexical scope of this function
                 # that is, in the environment of makeCacheMatrix.
                 # a "normal" assignment would create a new local variable that masked
                 # one in the lexical environment, and this would vanish on exit, so we
                 #don't do that.
      cached.inverse <<- NULL   # As we have changed our matrix, we no longer have a chached inverse.
   }
   
   # the get function omits the "{}" as it only contains one statement
   # It was done like this to confuse us poor students, but it didn't work!
   get <- function() x #return the lexically scoped variable x

   # Another single statement function. I included the braces to un-obfuscate
   # Cache the provided inverse matrix.
   # Note: no check is done to ensure it is correct but
   # all.equal(x %*% inverse, diag(nrow(x))) returns TRUE if the inverse is OK
   setinverse <- function(inverse) {
      cached.inverse <<- inverse  #save the function parameter in the environment of makeCacheMatrix.
   }
   
   # Again, simply return a value previously saved by setinverse()
   # If no such value has been saved, it will be NULL
   getinverse <- function() cached.inverse

   # The return value of makeCacheMatrix is a list of named functions
   # That is, if x = makeCacheMatrix(m), then x$set(), x$get(), x$setinverse() and x$getinverse() are defined.
   # These functions exist in the lexical scope of this function.
   # That is, they are contained in the environment of this function.
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Return a matrix that is the inverse of 'x'
## The parameter x is a special matrix object created with makeCacheMatrix().
## The function caches the result for efficiency reasons.

cacheSolve <- function(x, ...) {
   m <- x$getinverse()    # The matrix m is either the cached inverse previously stored in x,
                          # or it is NULL.
   if (!is.null(m)) {     # If we have a previously cached matrix in x, 
      message("setting cached inverse")
      return (m)          # Then return it
   }
   mat <- x$get()         
   m <- solve(mat, ...)   # else calculate it
   x$setinverse(m)        # cache it for future calls
   m                      # return it.
   # why not move this logic into makeCacheMatrix(), remove a source of error and simplify everything?
}
