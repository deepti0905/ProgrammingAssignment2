
#This function will cache matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <-NULL
        set<-function(val){
                x <<-y
                inverseMatrix<<-NULL
        }
        get<-function()x
        
        setInverse<-function(inverse) inverseMatrix <<-inverse        
        getInverse<-function() inverseMatrix
        
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## This function will calculate inverse and cache it. If inverse already cached, it will return cached value

cacheSolve <- function(x, ...) {
        inverseValue<-x$getInverse()
        if(!is.null(inverseValue))
        {
                message("getting cached inversed matrix")
                return(inverseValue)
        }
        data<-x$get()
        newInverse<-solve(data,...)
        x$setInverse(newInverse)
        newInverse
}

##Sample Run

# > source("cacheMatrix.R")
# > x=rbind(matrix(c(1,2,3,4),nrow=2,ncol=2))
# > x
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > m=makeCacheMatrix(x)
# > m
# $set
# function (val) 
# {
#         x <<- y
#         inverseMatrix <<- NULL
# }
# <environment: 0x0000000015ec9328>
#         
#         $get
# function () 
#         x
# <environment: 0x0000000015ec9328>
#         
#         $setInverse
# function (inverse) 
#         inverseMatrix <<- inverse
# <environment: 0x0000000015ec9328>
#         
#         $getInverse
# function () 
#         inverseMatrix
# <environment: 0x0000000015ec9328>
#         
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5