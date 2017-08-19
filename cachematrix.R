
## function to create cached Matrix

makeCacheMatrix <- function(x = matrix()) {
     mem<-NULL
     
     ### set and get functions
     set<-function(y){
          x<<-y
          mem<<-NULL
     }
     get <-function() x
     
     ### set and get for solve functions
     
     setSolved <-function(solved) mem<<-solved
     
     getSolved <-function() mem
     
     ### return scope/memory reference
     
     list(set=set,get=get,setSolved=setSolved,getSolved=getSolved)

}


## Function to solve/return inverse matrix from memory

cacheSolve <- function(x, ...) {

     ### Fetch the solve from memory     
     mem<-x$getSolved()
     
     ### If its not null return the result
     if(!is.null(mem)){
          message("getting cashed data")
          return(mem)
     }
     
     ### If it is null, calculate solve 
     
     message("not available in cache, solving ....")
     
     ### read original matrix
     data<-x$get()
     
     ### solve
     mem<-solve(data,...)
     
     ### store
     x$setSolved(mem)
     
     ### return result
     mem
}


## Test the result

### creating a 3 * 3 matrix
x<-numeric()
x<-rbind(c(2,4,5),c(10,-3,12),c(15,17,-3))
x

### Cache the matrix
xmat<-makeCacheMatrix(x)
xmat

### Fetch solve from cache...should calculate
cacheSolve(xmat)

### Fetch solve from cache...should return result from memory
cacheSolve(xmat)
