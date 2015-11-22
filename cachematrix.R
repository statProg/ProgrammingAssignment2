
## These functions calcualte the inverse matrix and retrieve the calculated matrix
## if the matrix was calculated previously.
makeCacheMatrix <- function(matrix = matrix()) {
  inverseMatrix<-NULL
  
  set<-function(y){
    matrix<<-y
    inverseMatrix<<-NULL
  }
  
  get<-function() matrix
  
  setInverseMatrix<-function(z) inverseMatrix<<- z
  
  getInverseMatrix<-function() inverseMatrix
  
  list(set=set, get=get,
       setInverseMatrix=setInverseMatrix,
       getInverseMatrix=getInverseMatrix)
}
## This function checks if the respective matrix is stored in cache. if yes and the value of the respective
## matrix is in the cache, the cached data is returned. If the matrix was not calculated previously, the matrix is
## calculated (the inverse of the matrix).
cacheSolve <- function(matrix=matrix(), ...) {
  inverseMatrix<-matrix$getInverseMatrix()
  if(!is.null(inverseMatrix)){
    print("get cached data...")
    return(inverseMatrix)
  }
  
  else {
    print("Calculate inverse matrix...")
    calcMatrix<-matrix$get()
    inverseMatrix<-solve(calcMatrix, ...)
    matrix$setInverseMatrix(inverseMatrix)
    inverseMatrix
  }
}
