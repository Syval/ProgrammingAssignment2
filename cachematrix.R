# Using the makeCacheMatrix function to make a special "matrix" object that can store a matrix and cache its inverse.
# This function creates a list that has four functions--Set, Get, SetInvrs, GetInvrs (see details below)

  makeCacheMatrix <- function(x=matrix())  
    {
        InvrsCch <- NULL                    # to store inversion result
        Set <- function(y)                  # to set a matrix to object that was created by the function makeCacheMatrix
          {                                     
	        x <<- y                         # to use the <<- operator to assign a value to our object in an envi diff fr current envi
	        InvrsCch <<- NULL               # to initialize InvrsCch to NULL
          }
        Get <- function() x                                              # to return the input matrix
        SetInvrs <- function(Invrs) InvrsCch <<- Invrs                   # to set the inversed matrix
        GetInvrs <- function() InvrsCch                                  # to return the inversed matrix
        list(Set = Set, Get = Get, SetInvrs = SetInvrs, GetInvrs = GetInvrs)     # to return a list with such functions. . . 
	}

  
# Using the cacheSolve function to computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cacheSolve should retrieve the inverse from the cache.

  cacheSolve <- function(x, ...) 
    {
        InvrsFxn <- x$GetInvrs()           # to get the inversed matrix from object x
        if (!is.null(InvrsFxn) )           # if the inversion result is there
	      {  
	    message ("getting cached data")
	    return (InvrsFxn)                  # return the inversion calculated
          }
        Target <- x$Get()                  # else we do x$Get to get the matrix object
        InvrsFxn <- solve(Target, ...)     # to use the solve function to compute the inverse of a square matrix
        x$SetInvrs(InvrsFxn)               # to set it to the object
        InvrsFxn                           # to return the result
    }
