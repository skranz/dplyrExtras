
examples.modify = function() {
  
  library(microbenchmark)
  library(modify)

  n = 10
  df = data.frame(a= sample(1:3,n,replace=TRUE),
                   b= sample(1:100,n,replace=TRUE),
                   x=rnorm(n))
  # Set x to 100 where a==2
  modify(df,a==2, x=100)
  df
  # Set x to the mean value of b*100 in each group of a
  modify(df,.by=c("a"),
         x=mean(b)*100)
  df
  
  # Call with strings
  com = "x=200"
  s_modify(df,"a==2", com)
  

  
  # Benckmark compared to directly using data.table or dplyr 
  n = 1e6
  df = data.frame(a= sample(1:5,n,replace=TRUE),
                   b= sample(1:100,n,replace=TRUE),
                   x=rnorm(n))
  dt = as.data.table(df)
  
  tbl = as.tbl(df)  
  modify(tbl, a==2,x = x+100)
  mutate(df, x=ifelse(a==2,x+100,x))
  
  microbenchmark(times = 5L,
    modify(tbl,a==2, x = x+100),
    modify(df,a==2, x = x+100),
    modify(dt,a==2, x = x+100),
    dt[a==2,x:=x+100],
    mutate.df = mutate(df, x=ifelse(a==2,x+100,x)),
    mutate.tbl = mutate(tbl, x=ifelse(a==2,x+100,x))
  )
  # Substantial speed increases compared to mutate with ifelse
  # and not much slower than directly using data.table syntax
  


}

EmptySymbol = function() (quote(f(,)))[[2]]

get.data.table.modify.call = function(args=NULL, filter.call=NULL, by=NULL, dat.quote=quote(dt)) {
  if (length(args)==1) {
    com = call(":=",names(args)[1],args[[1]])
  } else {
    com = as.call(c(list(quote(`:=`)),args))
  }
  
  if (is.null(filter.call)) {
    ca = call('[',dat.quote, EmptySymbol(),com )
  } else {
    ca = call('[',dat.quote, filter.call, com)
  }
  if (!is.null(by)) {
    ca$by = by
  }
  
  ca
} 

#' Fast in place modification of data.frames and data.tables
#' 
#' modify is essentially just a wrapper for data.table syntax but it can be used
#' for other data containers. It is thought as an addition to dplyr functions.
#' While the functionality can be replicated by mutate, modify can be much faster
#' and more concise if only values for selected rows shall be modified.
#'  
#' @param .data a data.frame, data.table or dplyr tbl object
#' @param .if optional a boolean conditions that specifies the rows that shall be modifed
#' @param .by optional a vector of column names used for computations that are splitted by groups
#' @param ... formulas for columns that are modified or newly created
#' @export 
modify = function(.data,.if,.by=NULL,..., .envir=parent.frame()) {

  .data = substitute(.data)
  args =  eval(substitute(alist(...)))
  #restore.point("modify")
  data.var = as.character(.data)
  dat = get(data.var,.envir)

  if (!is.data.table(dat)) {
    dt = as.data.table(dat)
  } else {
    dt = dat
  }

  if (missing(.if)) {
    filter.call=NULL
  } else {
    filter.call=substitute(.if)
  }
  ca = get.data.table.modify.call(args=args, by=.by, filter.call=filter.call)
  
  eval(ca)
  if (!is.data.table(dat)) {
    if (is.tbl(dat)) {
      dat = as.tbl(dt)
    } else {
      dat = as(dt, class(dat))
    }
    assign(data.var, dat, .envir)
  }
  invisible(dat)
}

#' Modified version of modify that uses string arguments
#' @export
s_modify = function(.data, ...) {
  eval.string.dplyr(.data,"modify", ...)
}
 