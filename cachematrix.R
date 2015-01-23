# The script contains two principal functions:
#        makeCacheMatrix: Create a special list of functions that can cache the inverse of a matrix and the original matrix, so it can be known
#                         if said object has changed.
#             cacheSolve: Compute the inverse of the special "matrix" returned by makeCacheMatrix, if the inverse has already been calculated 
#                         and the matrix has not changed the function retrieve the inverse from the cache. If the matrix has changed the 
#                         new inverse is calculated and the new matrix is stored to compare it the next time cacheSolve is called.



# Create a list of functions to store in cache the inverse of a matrix, 
# with the functions you can: get/set the matrix, get/set the inverse.

makeCacheMatrix <- function(x = matrix()) {

  mat  <- NULL
  resp <- x
  pon  <- function(y) {
          x <<- y
          mat <<- NULL
  }
  devu   <- function () resp
  respa  <- function (nva) resp <<- nva
  obt    <- function() x
  poninv <- function(solv) mat <<- solv
  obtinv <- function() mat
  
  
  list(pon = pon, 
       obt = obt,
       poninv = poninv,
       obtinv = obtinv, 
       devu = devu, 
       respa = respa)
}

# In this function the inverse of the matrix passed to makeCacheMatrix is
# calculated or retrieved from cache if the inverse was previously calculated
# in a previous execution of the function. The structure of the fucntion is:
# -- Retreive from cache the inverse, if the inverse exists and the matrix object
#    hasn't changed the inverse is returned.
# -- If the matrix has changed or doesn't exists we take the matrix and the inverse
#    is calculated.
# -- The new inverse and matrix are stored in cache for the next execution.

cacheSolve <- function(x, ...) {
        mat <- x$obtinv()
        
        if(!is.null(mat)) {
           if (isTRUE(all.equal(c,x$devu()))){
              message("Obtengo Matriz de Cache")
              return(mat)
           }
        }
        message("Se vuelve a calcular")
        x$pon(c)
        matriz <- x$obt()
        mat    <- solve(matriz)
        x$poninv(mat)
        x$respa(c)
        mat
}
