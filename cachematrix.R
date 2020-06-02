## This function calculates the inverse of a matrix and stores 
## the answer in memory. This is retrieved when needed. 'Retrieving' instead
## of 'caculating', saves precious resources, and makes the code 'better'.

makeCacheMatrix<-function(x=matrix()){
  I<-NULL
  set<-function(y){
    x<<-y
    I<<-NULL
  }
  get<-function()x
  setSolve<-function()I<<-solve(x)
  getSolve<-function()I
  list(set=set,get=get,
       setSolve=setSolve,
       getSolve=getSolve)
}

## This function solves for the inverse of matrix and also caches it. It
## returns "getting cached data" when retriving data.

cacheSolve<-function(x=matrix()){
  I<-x$getSolve()
  if(!is.null(I)){
    message('getting cached data')
    return(I)
  }
  data<-x$get()
  I<-solve(data)
  x$setSolve(I)
  I
}
