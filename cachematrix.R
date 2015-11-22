## This code has two functions. The first function (makeCacheMatrix) is able to 
## cache the inverse of an nxn Matrix. 
## The second function(cacheSolve) is able to return the inverse of matrix 
## from cache if the matrix is in cache.
## If the matrix is not in cache, the inverse is (re-)calculated. 

makeCacheMatrix <- function(matrix = matrix()) {
  inverseMatrix<-NULL
  
## clear cache and set new matrix  
  
  set<-function(y){
    matrix<<-y
    inverseMatrix<<-NULL
  }
  
## get matrix  
  get<-function() matrix

## save matrix (in cache)
 
  setInverseMatrix<-function(z) inverseMatrix<<- z

## get inverse Matrix from cache
  
  getInverseMatrix<-function() inverseMatrix
  
  list(set=set, get=get,
       setInverseMatrix=setInverseMatrix,
       getInverseMatrix=getInverseMatrix)
}
## This function checks if the respective matrix is stored in cache. if yes and the value of the respective
## matrix is in the cache, the cached data is returned. If the inverse matrix was not calculated previously, the matrix is
## calculated.

cacheSolve <- function(matrix=matrix(), ...) {
 
## check cache for inverse matrix (read and check)  
  
   inverseMatrix<-matrix$getInverseMatrix()
  if(!is.null(inverseMatrix)){
    print("get cached data...")
    return(inverseMatrix)
  }
  
## If inverse matrix is not in cache, it is recalculated. 
   
  else {
    print("Calculate inverse matrix...")
    calcMatrix<-matrix$get()
    inverseMatrix<-solve(calcMatrix, ...)
    matrix$setInverseMatrix(inverseMatrix)
    inverseMatrix
  }
}
