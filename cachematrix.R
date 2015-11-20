###
### OO style object which will calculate the inverse of the supplied matrix
###
### setOriginalMatrix set the input matrix
### getOriginalMatrix get the input matrix
###
### setInverseMatrix set the output matrix (inverse of original)
### getInverseMatrix get the output matrix (inverse of original)
###
makeCacheMatrix <- function(x = matrix()) #constructor
{
  #init
  mInverseMatrix <- NULL
  mOriginalMatrix <- x
  
  #setter: set value into mOriginalMatrix
  setOriginalMatrix <- function(newMatrix) 
  {
    mOriginalMatrix <<- newMatrix
    mInverseMatrix <<- NULL
  }
  
  #getter: get value from mOriginalMatrix
  getOriginalMatrix <- function() mOriginalMatrix  

  #setter: set value into mInverseMatrix
  setInverseMatrix <- function(inverseMatrix) mInverseMatrix <<- inverseMatrix

  #getter: get value from mInverseMatrix
  getInverseMatrix <- function() mInverseMatrix
  
  #export functions via a list
  list(setOriginalMatrix = setOriginalMatrix, 
       getOriginalMatrix = getOriginalMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

### 
### This function computes the inverse of the special "matrix" returned by the makeCacheMatrix object.
### Returns: a matrix that is the inverse of the input param 'x'
###
cacheSolve <- function(x, ...) 
{
  
  #get matrix from makeCacheMatrix object
  matrixInverse <- x$getInverseMatrix()
		
  #if matrixInverse not null i.e. cached then return it
  if(!is.null(matrixInverse)) 
  {
    message("getting cached data")
    return(matrixInverse)
  }
  
  #get original matrix
  matrixOriginal <- x$getOriginalMatrix()
  
  #calculated inverse
  matrixInverse <- solve(matrixOriginal)
  
  #cache the inverse
  x$setInverseMatrix(matrixInverse)
  
  #return calculated inverse
  matrixInverse
  
}
