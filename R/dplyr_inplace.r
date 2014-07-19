
examples.remove_cols = function() {
  dt = data.table(a=1:3, b=1:3, c=4:6)
  dt
  remove_cols(dt,c("a" ,"b"))
  dt
}

#' Inplace remove columns from a data.table
#' @param dt a data.table
#' @param cols a string with column names
#' @export
remove_cols = function(dt, cols) { 
  for (col in cols) {
    eval(substitute(dt[, col:=NULL], list(col=col)))
  }
  invisible(dt)
}
