## makeCacheMatrix - function creates a matrix that can cache its inverse
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the mean
## 4.get the value of the mean
# return List(....)
makeCacheMatrix <- function(x = matrix()) 
{
# x: a square matrix
cashed_matrix <- NULL 

set   <- function(y) 
         {x  <<- y 
          cashed_matrix <<- NULL 
         } 
get    <- function() x 

setInversed_matrix <- function(inversed_matrix) cashed_matrix <<- inversed_matrix  
getInversed_matrix <- function() cashed_matrix 

list(set=set, get=get, setInversed_matrix=setInversed_matrix, getInversed_matrix=getInversed_matrix)
}


## cacheSolve() accept 'x'-List parameters
## one of them is matrix, can be get as x$getInversed_matrix
## Return a matrix that is the inverse of 'x'-matrix
cacheSolve <- function(xList, ...) 
{
# Try get cashed matrix if exist already.
existed_matrix = xList$getInversed_matrix()

# if the inverse has already been calculated
if (!is.null(existed_matrix))
   {# get it from the cache and skips the computation. 
    message("get from cached data")
    return(existed_matrix)
   }

# continue if cashed matrix NOT exist, 
# calculates the inversed 'x'-matrix accepted from x-List parameters  
tmp_matrix = xList$get()
#R funcion = solve(A): Inverse of A where A is a square matrix. 
new_matrix = solve(tmp_matrix)

# sets the result of the inverse in the variable of xList.
xList$setInversed_matrix(new_matrix)

new_matrix
}


## example #1 from:
## http://www.mathwords.com/i/inverse_of_a_matrix.htm
##
#> M0 = matrix( c(4,3,3,2), nrow=2, ncol=2)
#> M0
#     [,1] [,2]
#[1,]    4    3
#[2,]    3    2
# create List with parameters:
#>mlist <-makeCacheMatrix(M0)
# see saved started matrix in list
#> mlist$get()
#     [,1] [,2]
#[1,]    4    3
#[2,]    3    2
# be sure cashed matrix not exit:
#> mlist$getInversed_matrix()
#NULL
# create inversed matrix:
#> mI <- cacheSolve(mlist)
#> mI
#     [,1] [,2]
#[1,]   -2    3
#[2,]    3   -4
#
# Now prove that inversed matrix 'mI' counted right:
#> M0 %*% mI
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
#
# Yes!!! Diagonal = 1,1,....1
#
# See now cashed inversed matrix:
#> mlist$getInversed_matrix()
#     [,1] [,2]
#[1,]   -2    3
#[2,]    3   -4
#
#> mlist$get()
#     [,1] [,2]
#[1,]    4    3
#[2,]    3    2
#
# And again run
#> mI_2 <- cacheSolve(mlist)
#getting cached data
#> mI_2
#     [,1] [,2]
#[1,]   -2    3
#[2,]    3   -4

###########################################################
# example #2 from:
# http://www.purplemath.com/modules/mtrxinvr.htm
# create 3*3 matrix
#> M3 = matrix( c(1,1,1,3,4,3,3,3,4), nrow=3, ncol=3)
#> M3
#     [,1] [,2] [,3]
#[1,]    1    3    3
#[2,]    1    4    3
#[3,]    1    3    4
# and repeat all steps  as in first example
#
#> mlist3 <-makeCacheMatrix(M3)
#
#> mlist3&get()
# mlist3$get()
#     [,1] [,2] [,3]
#[1,]    1    3    3
#[2,]    1    4    3
#[3,]    1    3    4
#
#> mlist3$getInversed_matrix()
#NULL
#
#> mI <- cacheSolve(mlist3)
#> mI
#     [,1] [,2] [,3]
#[1,]    7   -3   -3
#[2,]   -1    1    0
#[3,]   -1    0    1
#> mlist3$getInversed_matrix()
#     [,1] [,2] [,3]
#[1,]    7   -3   -3
#[2,]   -1    1    0
#[3,]   -1    0    1
#> mlist3$get()
#     [,1] [,2] [,3]
#[1,]    1    3    3
#[2,]    1    4    3
#[3,]    1    3    4
#
#> M3 %*% mI
#     [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1
#
# YES !!! Diagonal = 1,1,1,....,1
#
#> mI <- cacheSolve(mlist3)
#getting cached data
#> mlist3$get()
#     [,1] [,2] [,3]
#[1,]    1    3    3
#[2,]    1    4    3
#[3,]    1    3    4
#> mlist3$getInversed_matrix()
#     [,1] [,2] [,3]
#[1,]    7   -3   -3
#[2,]   -1    1    0
#[3,]   -1    0    1
#
## end ################################################
