 
# Tools to make it run
deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
}
 
dt_env <- function(dt, env) {
  env <- new.env(parent = env, size = 2L)
  env$dt <- dt
  env$vars <- deparse_all(groups(dt))
   
  env
}
 
 
 
# code for manip.r
 
#' mutate selected rows
#' 
#' change values of columns only in rows that satisfy the .if condition
#' Note: you cannot create new columns with mutate_rows but only change
#' values in selected rows of existing columns
#' 
#' @param .data the data
#' @param .if a logical condition that selects rows, e.g. a=="B"
#' @param ... the command to mutate existing columns
#' @export
mutate_rows = function (.data,.if,...) {
  UseMethod("mutate_rows")
}


#' mutate selected rows
#' 
#' Old pseudonym for mutate_rows.
#' Originally the function mutate_rows was called mutate_if,
#' however, dplyr 5.0 introduced also a function called mutate_if
#' that does something different however. 
#' I now have depricated the synonym mutate_if give 
#' a warning and call dplyr:mutate_if
#' @param .data the data
#' @param ... the command to mutate existing columns
#' @export
mutate_if = function (.data,...) {
  res = try(dplyr::mutate_if(.data,...))
  if (is(res, "try-error")) {
    msg = as.character(res)
    msg = paste0(msg, "\nNote that you called the function mutate_if in dplyrExtras instead of dplyr. If you wanted the dplyrExtras version note that it is deprecated since dplyr meanwhile also introduce a function mutate_if with different functionality. Use mutate_rows in dplyrExtras instead.")
    stop(msg)
  }  
  res
}
 
 
# for tbl-data.frame.R

#' @export
mutate_rows.data.frame =function (.data,.if,...)
{
  dt = as.data.table(as.data.frame(.data))
  .if.quoted = substitute(.if)
  as.data.frame(mutate_rows.data.table(.data=dt,.if.quoted=.if.quoted,...,inplace=TRUE, .parent.env = parent.frame()))
}
 
# for manip-df.r
 

#' @export
mutate_rows.tbl_df <- function (.data,.if,...) {
  dt = as.data.table(as.data.frame(.data))
  .if.quoted = substitute(.if)
  tbl_df(mutate_rows.data.table(.data=dt,.if.quoted=.if.quoted,...,inplace=TRUE, .parent.env = parent.frame()))
}
 
#' @export
mutate_rows.tbl_dt <- function(.data,.if, ...) {
  .if.quoted = substitute(.if)
  tbl_dt(
  mutate_rows.data.table(.data=.data,.if.quoted=.if.quoted,...,inplace=TRUE, .parent.env = parent.frame())
  )
}
 
# for manip-dt.r
 

#' @export
mutate_rows.data.table <- function (.data,.if, ..., inplace = FALSE,.if.quoted=NULL, .parent.env=parent.frame())
{
  if (is.null(.if.quoted))
  .if.quoted = substitute(.if)
   
  if (!inplace)
  .data <- copy(.data)
  env <- new.env(parent = .parent.env, size = 1L)
  env$data <- .data
  cols <- named_dots(...)
  #cols = eval(substitute(alist(...)))
   
  for (i in seq_along(cols)) {
  call <- substitute(data[.if.quoted, `:=`(lhs, rhs)], list(lhs = as.name(names(cols)[[i]]), rhs = cols[[i]], .if.quoted =.if.quoted))
  eval(call, env)
  }
  .data
}
 
 
 
# for manip-grouped-dt.r
 

#' @export
mutate_rows.grouped_dt <- function(.data,.if, ..., inplace = FALSE, .if.quoted=NULL) {
  data <- .data
  if (is.null(.if.quoted))
  .if.quoted = substitute(.if)
  if (!inplace) data <- copy(data)
   
  env <- dt_env(data, parent.frame())
  cols <- named_dots(...)
  # For each new variable, generate a call of the form df[, new := expr]
  for(col in names(cols)) {
    call <- substitute(dt[.if.quoted, lhs := rhs, by = vars],
    list(lhs = as.name(col), rhs = cols[[col]], .if.quoted=.if.quoted))
    eval(call, env)
  }
 
  grouped_dt(
    data = data,
    vars = groups(.data)
  )
}
 

#' @export
mutate_rows.grouped_df <- function(.data,.if, ...) {
  # This function is currently extremely unelegant and inefficient
  # Problem: when transforming to data.table row order will be changed
  # by group_by operation at least in dplyr 0.1.3
  # So I manually restore the original row order
  if (NROW(.data)==0)
    return(.data)
  .if.quoted = substitute(.if)
  vars = groups(.data)
  dt = as.data.table(as.data.frame(.data))
  class(dt) = c("data.table","data.frame")
  # does not seem to work correctly
  #mutate(dt, INDEX.ROW__ = 1:NROW(.data), inplace=TRUE)
  dt$INDEX.ROW__ = 1:NROW(.data) # slower but seems to work
  gdt = grouped_dt(dt, vars=vars)
  gdt = mutate_rows.grouped_dt(gdt,.if.quoted=.if.quoted,..., inplace=TRUE)
  data = dplyr:::grouped_df(data=as.data.frame(gdt), vars=vars)
  # restore original order
  data = select(arrange(data, INDEX.ROW__), -INDEX.ROW__)
  data
 
}
 
 
 
examples.mutate_rows = function() {
  library(microbenchmark)
  library(dplyr)
  library(data.table)
  library(dplyrExtras)

  # create a data
  set.seed(123456)
  n = 10
  df = data.frame(a= sample(1:3,n,replace=TRUE),
  b= sample(1:100,n,replace=TRUE),
  x=rnorm(n))
  dt = as.data.table(df)
  
  # different calls to mutate_rows
  mutate_rows(df,a==3,y=100)
  mutate_rows(tbl_df(df),a==1,x=200)
  mutate_rows(as.tbl(df),a==1,x=300,b=400)
  mutate_rows(dt,a==1 | a==2,x=400)
  mutate_rows(group_by(dt,a),a==1 | a==2,x=mean(b)) 
  mutate_rows(group_by(df,a),a==1 | a==2,x=mean(b))

  # if you create a new column rows that don't
  # match the if condition have an NA
  mutate_rows(df,a==3,z=100)
  
  # You can only have one if condition in a mutate_rows call
  # So multiple changes require nesting or piping
  library(magrittr)
  df %>% mutate_rows(a==3,z=300) %>%
         mutate_rows(a==2,z=200) 
  
  # For historical reasons there is also still the synonym
  # mutate_if. But not that dplyr 5.0 has introduced its own
  # mutate_if function with quite different functionality
  
  mutate_if(df,a==3,y=99)
  
    
  
  # Small benchmark: compare with mutate + ifelse
  n = 1e6
  df = data.frame(a= sample(1:3,n,replace=TRUE),
  b= sample(1:100,n,replace=TRUE),
  x=rnorm(n))
  microbenchmark(times = 5L,
    mutate(df, x=ifelse(a==2,x+100,x)),
    mutate(df, x=if_else(a==2,x+100,x)),
    mutate_rows(df, a==2, x=x+100)
  )
#             mean
# ifelse      540.7145
# if_else     360.4928
# mutate_rows 114.3891
}