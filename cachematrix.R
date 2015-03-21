
#The makeCacheMatrix function is used to cache the value of the matrix, thereby preventing unnecessary
#calculations that will save both time and memory. It contains the get and set methods used to 
#modify the state of the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  inv<-NULL
  set<-function(temp2)
  {
    x<<-temp2
    inv<<-NULL
  }
  get<-function(){ x }                            #function to return the present matrix
  getInverse<-function(){ inv }                      #function to return the present inverse
  setInverse<-function(invt){ inv<<-invt }           #function to update the inverse
  
  #Returning a list of functions
  list(set=set,get=get,getInverse=getInverse,setInverse=setInverse)
  
}


#The cacheSolve() method is used to obtain the value of the Inverse of the matrix. In case the matrix
#remains the same and the inverse is already calculated, the cached value of the matrix inverse is
#returned.

cacheSolve <- function(x,...) 
{
  inv<-x$getInverse()
  
  #checking if the inverse has already been calculated
  if(!is.null(inv))
  {
    print("Fetching the Cached Value")
    return(inv)
  }
  data<-x$get()  
  inv<-solve(data)       #Finding the inverse
  x$setInverse(inv)      #Updating the Inverse
  inv
  ## Return a matrix that is the inverse of 'x'
}
