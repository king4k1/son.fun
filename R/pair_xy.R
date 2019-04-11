
pair_xy <- function(x, y){
  n.x <- length(x)
  n.y <- length(y)
  y_result <- c()
  for(i in 1:n.y){
    y_result <- c(y_result, rep(i, n.x))
  }
  result <- data.frame(x = rep(1:n.x, n.y),
                       y = y_result)
  result
}

###test with below term###
```
x <- letters[1:4]
y <- LETTERS[1:5]
pair_xy(x,y)
```