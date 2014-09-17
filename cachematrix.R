## These functions will return a matrix that is the inverse of x,
## assuming the input x is an invertable matrix. It will store
## previously used matricies and their inverses so that if the
## functions are called again later, the matrix inverse can been
## looked-up in memory rather than re-computed

## makeCacheMatrix() creates a list which contains functions to
## calculate the inverse of the matrix, and store the matrix and 
## its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  #create an empty matrix with the same dimensions as your input matrix
  z<-matrix(nrow=nrow(x),ncol=ncol(x))
  
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    z <<- matrix(nrow=nrow(x),ncol=ncol(x))
  }
  
  #get the value of the matrix
  get <- function() x
  #set the function used to find the matrix inverse
  setinv <- function(solve) z <<- solve
  #get the value of the inverse of the matrix
  getinv <- function() z
  #create a list containing the four components we want to store
  #for each matrix in our cache
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
#*************************************************************#


## cacheSolve() will first check to see if the matrix inverse has
## already been computed and stored. If so, cacheSolve() will
## return the previously computed inverse. If not, cacheSolve()
## will solve for the inverse and store the matrix in the cache.

cacheSolve <- function(x, ...) {
  #check to see if the inverse matrix already exists
  z <- x$getinv()
  #if the inverse already exists, grab it from the cache
  if(!is.na(z[1,1])) {
    message("getting cached data")
    #this part will break out of the function 
    #and return the pre-computed inverse
    return(z)
  }
  #if the inverse does not already exist in the cache
  data <- x$get()
  #calculate the matrix inverse
  z <- solve(data, ...)
  #save the new matrix inverse in the cache
  x$setinv(z)
  #return z, the matrix inverse
  z
}