## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.


##  makeCacheMatrix creates a vector, which is a list containing functions to:
##  1. Set the value of the Matrix
##  2. Get the value of the Matrix
##  3. Set the value of the Inverse of the Matrix
##  4. Get the value of the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL                                     ## Set m (i.e. variable which stores inverse of input matrix) to NULL
  set<-function(y){                   
    x<<-y                                     ## caches x (i.e. the input matrix) in the set function environment
    m<<-NULL                                  ## caches m with the value as NULL
  }
  get<-function() x                           ## returns the value of x and stores it into the variable get
  setinverse<-function(solve) m<<- solve      ## caches m and assigns it the value of the inverse of input matrix
  getinverse<-function() m                    ## returns the value of m and stores it into the variable getinverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## creates a list containing the functions to do all 4 of the above mentioned tasks)

}


## The following function calculates the mean of the special "vector" created with the above function. 
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.


cacheSolve <- function(x, ...) {
   m<-x$getinverse()                           ## looks for cached value of inverse of matrix
  if(!is.null(m)){                            ## this condition returns the cached matrix if the values of cached matrix is not NULL
    message("getting cached data")
    return(m)
  }
  matrix1<-x$get()                            ## In case m is null, we assign the values of input matrix to the variable matrix 1
  m<-solve(matrix1, ...)                      ## Variable m is assigned the value of inverse of matrix1
  x$setinverse(m)                             ## cache the above calculated value of inverse (in variable m)
  m                                           ## returns the value of m
}