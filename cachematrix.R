# REFERENCES:
#============
# https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/mowodw4rEemS0Q4U5mc4kg

rm(list = ls())  # remove any currently existing objects in the current R session

# define makeVector and cachemean

makeMatrix <- function( rows, cols ) {
        mymatrix <- matrix( rnorm(1:100), rows, cols ) # create a matrix of given rows and columns.
        message("I created the matrix for the first time\n")
        message("Line 12\nData structure of mymatrix.\n")
        str(mymatrix)
        message("\nPrint mymatrix to the console.\n")
        print(mymatrix)

        cachedInv <- NULL # set the value of the cached Inverse matrix to NULL. This is the first time we create the matrix,
        # so, there isn't an inverse yet.
        
        set <- function( y ) { #assigns the 'y' matrix to mymatrix.
                mymatrix <<- y
                cachedInv <<- NULL
        }

        get <- function(){
                mymatrix # returns mymatrix.
        } 

        setInv <- function( calcInv ) {
                cachedInv <<- calcInv # assignes a calculated Inverse to cachedInv when there is one calculated.
        }

        getInv <- function() {
                cachedInv # returns cachedInv.
        }

        list(set = set, get = get, setInv = setInv, getInv = getInv)
}
#
#
cacheInvMatrix <- function( mymatrix, ...) {
    
    calcInv <- mymatrix$getInv() # takes the cached Inverse, initially NULL, and assigns it to the calculated Inverse.

    if( !is.null( calcInv )) { # if calc Inve != NULL, it was calculated before, just return its value.
            cat("\n","Line 35","\n")
            message("Getting cached data. The Inverse is not calculated.")
            calcInv
            
    } else { # else, the inverse has not been calculated before, calculate it now.
            dataMatrix <- mymatrix$get() # gets mymatrix into dataMatrix.
            calcInv <- solve(dataMatrix, ...) # solve calculates the Inverse of dataMatrix.
            mymatrix$setInv( calcInv ) # sets the Inverse value to cache to the calculated value of the inverse.
            cat("\n","Line 43","Calculating the Inverse","\n")
            calcInv
    }
}

# Create a matrix to test the code:
matrixRows <- 4
matrixCols <- 4
makeMatrix.object <- makeMatrix( matrixRows, matrixCols )

# as noted in Len’s article, the parent environment
# of the set function is the 
# makeMatrix.object environment
# so we can use this information to list 
# what objects are in
# the makeMatrix.object environment

ls(envir = environment( makeMatrix.object$set))
# [1] "cachedInv" "cols"      "get"       "getInv"    "mymatrix"  "rows" "set"       "

cat("\n","line 75","\n")
get("cachedInv", envir = environment( makeMatrix.object$set ))
# NULL
# OK since that was how cachedInv was defined in makeMatrix

cat("\n","line 80","Print the initial matrix.\n")
get("mymatrix", envir = environment( makeMatrix.object$set ))

# this is the first time we calculate the Inverse by calling cacheInvMatrix. Line 43 is printed.
cacheInvMatrix( makeMatrix.object)
# same as calling: environment( makeMatrix.object$set)$cachedInv

# The next time we call the function, the inverse is not calculated, and line 35 is printed.
cacheInvMatrix( makeMatrix.object )
