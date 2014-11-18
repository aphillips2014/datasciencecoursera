add2 <- function(x,y) 
{
   x+y
}

#na.rm = FALSE means the missing values will not be included
#na.rm = TRUE means the missing values will be included

cube <- function(x, n){
  x^3
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}



y <- 10

f <- function(x) {
  y <- 2
  y^2 + g(x)
}

g <- function(x) { 
  x*y
}

