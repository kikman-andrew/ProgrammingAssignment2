makeCacheMatrix <- function(a=matrix()){
  inv <- NULL
  set <- function(a){
      a <<- b
      inv <<- NULL
  }
  get <- function(){a}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function(){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(a, ...){
  inv <- a$getInverse()
  if (!is.null(inv)){
    message("loading cache")
    return(inv)
  }
  mat <- a$get()
  inv <- solve(mat, ...)
  a$setInverse(inv)
  inv
}
