## About cachematrix.R: It contains 2 function as described below and can be used to 
## calculate, cache and display inverse of n x n matrix.
## Created by: Umesh Rege
## Date: 23 September, 2015
## Features: 
##	1. User can specify value of n (of n x n matrix), along with actual matrix values
##	2. If user tries to define vector of type other than numeric, execution is halted
##	3. If length of vector is greater or less than n x n (matrix size) execution is 
##	   halted with appropriate error message
## About function makeCacheMatrix: The function takes 2 parameters 
## n = number of rows and columns of matrix
## x = numeric vector equivalent to n x n matrix
## inv = object to store inverse of matrix
## Additional comments are inline in the code

makeCacheMatrix<-function(n=numeric(),x=numeric()) {
	inv<-NULL			## Initialize inv object to NULL
	
	## Following IF condition checks if the vector is of type numeric only
	## execution stops if vector is of any type other than numeric	

	if(class(x) != "numeric") {	
		stop("Only vector of type numeric is allowed")
	}
	
	## Following IF condition checks if the length of the vector
	## is equal to n*n (square matrix), if its false execution is stopped
	## with appropriate message
	
	if(length(x) != n*n) {
		stop("\n Number of elements required: ",n*n,"\n Number of elements provided: ",length(x))
	}
		
	x<-matrix(x,nrow=n,ncol=n)	## Create matrix of size n x n with vector x
	
	set<-function(n,y) {		## The set() function which gives ability to change the matrix
	x<<-matrix(y,nrow=n,ncol=n)	## Set value of object x in Global environment with 
					## values provided in set function
	inv<<-NULL			## Set value of object inv in Global environment to NULL
 	}
 
 	get<-function() x		## The get() function returns original matrix
 	setinv<-function(inv) inv<<-inv	## The setinv() function is used to cache inverse of the matrix
					
 	getinv<-function() inv		## The getinv() function returns stored value of inverse of matrix
					## on demand (inv object)
 
	list(set=set,get=get, setinv=setinv,getinv=getinv)
}

## About function cacheSolve: The function takes vector as input
## creates the inverse of the input matrix if its NOT available in cache
## If its available in cache, it returns the cached version of Inverse of matrix

cacheSolve<-function(x,...) {
	inv<-x$getinv() 		## Fetches and assigns the value of the inverted matrix 
					## stored in getinv() function
	
	if(!is.null(inv)) {		## This IF condition checks if the value fetched and stored in
 	message("getting cached data")	## inv object in previous step is NOT NULL
 	return(inv)			## In this scenario, it prints message to user that
 	}				## it is serving cached inverse matrix and returns cached inverse matrix
 	
	data<-x$get()			## If the above IF statement is FALSE i.e. there is no cached inverse matrix
 	inv<-solve(data)		## inverse of matrix is calculated and stored in inv object and
 	x$setinv(inv)			## caches the inverse of matrix
 	inv
}
