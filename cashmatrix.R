mCmx <- function(x = matrix()) {
  mIn <- NULL
  set <- function(y){
    x <<- y
    mIn <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) mIn <<- inverse
  getInverse <- function() mIn 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


cSol <- function(x, ...) {
  mIn <- x$getInverse()
  if(!is.null(mIn)){
    message("getting cached data")
    return(mIn)
  }
  mat <- x$get()
  mIn <- solve(mat,...)
  x$setInverse(mIn)
  mIn
}

