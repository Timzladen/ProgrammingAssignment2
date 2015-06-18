##makeCacheMatrix stores a list of functions and creates an environment in which cache solve works

makeCacheMatrix<-function(x=matrix()){
  m<- NULL
  set<- function(y) {  #with set we set the environment of the matrix
    x<<-y
    m<<- NULL
  }
  get<- function() x    #with get we return the matrix
  setinverse<-function(solve) m<<- solve  #with setinverse we set the inverse value of the matrix
  getinverse<- function() m   #with getinverse we return the inversed matrix value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##This returns the inversed of x
cachesolve<-function(x, ...){
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
}