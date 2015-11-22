makeCacheMatrix <- function(matrix = matrix()) { # Explanations
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
