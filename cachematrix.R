## Functions to Create a Matrix in Cache and calculate the inverse of it

makeCacheMatrix <- function(x = matrix()) {
  ## The below function takes a Square Matrix and creates a list containing functions to 
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## x is the output of makeCacheMatrix
  i <- x$getinv()
  # if the inverse has already been calculated
  if(!is.null(i)) {
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(i)
  }
  # Otherwise calculates and sets the inverse value in Cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  
  # Returns the inverse of the matrix
  return(i)
}

## Function to test caching
testCaching = function(mat){
  ## mat - a Square invertible matrix
  
  #Calling makeCaheMatrix and storing the value in y
  y = makeCacheMatrix(mat)
  
  #Calling the cacheSolve by passing y and calculating the time it takes to return the value
  start.time = Sys.time()
  cacheSolve(y)
  dur = Sys.time() - start.time
  print(dur)
  
  #Calling the cacheSolve by passing y and calculating the time it takes to return the value
  start.time = Sys.time()
  cacheSolve(y)
  dur = Sys.time() - start.time
  print(dur)
}

#Creating a matrix of 1000X1000 and passing it to testCaching
set.seed(1)
x <- rnorm(1000000,1)
mat = matrix(x,nrow = 1000, ncol = 1000)
testCaching(mat)
