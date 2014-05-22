## Here are two functions to handle the caching of the inverse of a special matrix object.
## R Programming assignment 2.
## Yes, I know there are too many comments that clutter the code. It is but an exercise.

## makeCacheMatrix handles the creation of the special matrix object.
## It takes a single optional argument, a matrix on which the object is based, which defaults to
## a 1x1 matrix containing NA.
## It returns a list containing the special matrix objects methods 
## set(m) : set or change the matrix to matrix m
## get()  : get the base matrix
## setinverse(inv) : cache the matrix inv as the inverse matrix
## getinverse()    : get the cached matrix, or NULL if none has been set.
## NOTE: it is possible to use setinverse() to set an invalid value for the cached inverse!
## This is designed to be used with cacheSolve that ensures this does not happen.
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
   # If this is called via cacheSolve, it will be correct.
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
   invisible(list(set=set, get=get, setinverse=setinverse, getinverse=getinverse))
}


## Return a matrix that is the inverse of 'x'
## The parameter x is a special matrix object created with makeCacheMatrix().
## The function caches the result for efficiency reasons.
cacheSolve <- function(x, ...) {
   inv <- x$getinverse()    # The matrix m is either the cached inverse previously stored in x,
                            # or it is NULL.
   if (!is.null(inv)) {     # If we have a previously cached matrix in x, 
      #message("setting cached inverse") # For debugging
      return (invisible(inv)) # Then return it
   }
   mat <- x$get()           # else get the original matrix 
   inv <- solve(mat, ...)   # calculate the inverse (for this assignment we assume matrix is invertible)
   x$setinverse(inv)        # cache it for future calls
   invisible(inv)           # return it. (but don't print it)
}
