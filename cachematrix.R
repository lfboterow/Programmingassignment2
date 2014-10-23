# Cacheing Inverse of a Matrix
# setinverse: returns inverse of a matrix
# getinverse: gets the inverse
# set copies local matrix into a global X variable
# get brings back X, the matrix on the global variable

## Write a short comment describing this function

# makecachematrix takes a matrix as parameter, in this case an empty matrix by default
makeCacheMatrix <- function(x = matrix())   
	{
	m<-NULL
	set<-function(y)
		{
		x<<-y
		m<<-NULL
		}
	# get returns x, same as     get<-function() {x} 
	get<-function()            x
	setinverse<-function(solved) m<<-solved
	getinverse<-function()         m
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## CacheSolve returns the inverse of a matrix
# if the result doesn't exists, calls for getinverse to get it
# if the result is cached, brings the cached matrix, so it doesn't have to recalculate it
cacheSolve <- function(x, ...) {
	  m<-x$getinverse()
	  if(!is.null(m))
		{
		message("Getting cached data")
		return(m)
		}
	  data<-x$get()
	  m<-solve(data,...)
	  x$setinverse(m)
	  m
        ## Return a matrix that is the inverse of 'x'
}

#lfboterow@gmail.com