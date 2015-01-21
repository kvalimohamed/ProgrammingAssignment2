

## "makeCacheMatrix" works in conjuction with the function "cacheSolve" (defined below)
##  in an attempt to improve performance in solving equations that are computationally intensive
##  caching the inverse of a very large matrix rather than computing it repeatedly may save time
##  if the elements in the matrix have not changed, accessing the inverse that has been cached   


makeCacheMatrix <- function(x = matrix()) {

  
  ##  "makeCacheMatrix"  creates a special "matrix" object that keeps track of the data matrix (matData) and 
  ##   its inverse (matInv). this includes functions/methods to get and set the matrix and the inverse:
  ## 
  ##      set: sets the matrix in the cache, initialize the inverse to null, however, if the data matrix
  ##             has not changed from the last call to 'set', then the inverse will not be nullified.
  ##      get: get the matrix from cache
  ##      getInv: get the inverse of the matrix, 
  ##      setInv: set the inverse of the matrix in cache. 
  
  ##   limitations/side effects: 
  ##     it is assumed that the calling function has checked that matrix is square and had valid data
  ##     if makeCacheMatrix is called, the inverse is set to NULL
  ##     if no argument is passed to the function, matrix data is initialized to 1x1 element = NA
  ##     if there is change in the state of the data matrix, the inverse is set to NULL

  ##  define variables used for the inverse (matInv) and the data matrix (matData) 
  ##  these are associated with the instance of the object
  
  matInv <- NULL
  matData <- x
  

  ## "set function" - cache the matrix and set the inverse to null, however, 
  ##                  if the state of the data matrix have not changed (object is indentical)
  ##                     the Inv will not be nullified.
              
  
  set <- function(y) {
  
    if (!identical(matData,y)) {
      matData <<- y
      matInv <<- NULL
      message("Matrix data updated, inverse stored in cache is nullified")
    }  else {
      message("Warning: no change in state of data") 
    }

  }
  
  
  ##   get function - return the main matrix
  
  get <- function () matData
  
  

  ##   set the inverse, it is assumed that the calling function has check for correctness / validity
  
  setInv <- function(y)  matInv <<- y
  
  
  ##   get the inverse,  pull out the value in cache, if inverse was not set, this value will be null
  
  getInv <- function()  matInv
  

  # establish pairs listed for this object 
  
  list (set = set, get = get, 
        setInv = setInv, 
        getInv = getInv)
}




##   `the function defined below, "cacheSolve", computes the inverse of the special  
##     "matrix" of the type created by "makeCacheMatrix" defined above. If the inverse has
##     already been calculated (and the matrix has not changed), then
##     `cacheSolve` will retrieve the inverse from the cache, otherwise the Solve funtion will be used
##     to compute the inverse matrix. 


cacheSolve <- function(x, ...) {

  ## X is an object that is of the type 'makeCacheMatrix' 
  ## value returned is inverse matrix - either obtained from cache or solved explicitly 
  ## upon solving the inverse, store the matrix is stored in the cache 
  
  ##  The inverse of a square matrix is obtained by using the `solve`function in R.
  ##    if `X` is a square invertible matrix, then  `solve(X)` returns its inverse.
  ##    however, prior to calling solve, the function checks that data is square matrix w/o NA values

  ## Limitations/side effects: 
  ##    if the data is not square or has NA values, the function will stop.
  ##    this function assumes, object "x" has been created using 'makeCacheMatrix'

  


  cData <- x$getInv()
  
  ##  check to see if the cache has non-NULL value, if so return that. 

  if (!is.null(cData)) {
    
    message("Retrieving inverse from cache")
    return(cData)
    
  }  
  
  ##  At this point: 
  ##         Data was not in cache, 
  ##         Use the solve function to compute the inverse
  ##         Check that a data is valid 
  
  ## check if matrix is square
  
  MatData <- x$get()
  
  if (!is.matrix(MatData)) {
    stop("Data is not a matrix - unable to solve for inverse")
  }
  
  ##  check for square matrix 
  
  nr <- dim(MatData)[1]
  nc <- dim(MatData)[2]
  if (nr != nc) {
    stop("Data must be a square matrix - unable to solve for inverse")
  }
  
  if (any(is.na(MatData))) {
    stop("Data has NA values - unable to solve for inverse")
  }

  ##  data appears to be correct form, solve it and store in cache
  
  message("Computing the inverse")
  
  invMat <- solve(MatData)
  x$setInv(invMat)
  invMat
}


