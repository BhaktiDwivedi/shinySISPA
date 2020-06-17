filterVars = function(x,y) {
  if(missing(x)){
    stop("input data is missing!")
  }
  if(missing(y) | !is.vector(y)){
    stop("data needs to be a vector!")
  }
  return( x[ y>"0", ] )
}