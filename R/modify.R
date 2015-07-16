
examples.modify = function(a=0,K=0,n=0,x=0,y=0) {
  library(microbenchmark)

  K = 3
  n = 10
  dt = data.table(a= sample(1:3,n,replace=TRUE),
                   b= sample(1:100,n,replace=TRUE),
                   x=rnorm(n))
  df = as.data.frame(dt)
  # Set x to 100 where a==2
  modify(dt,a==2, y=x+100, z=y+K)
  
  modify(df,a==2,y=200,z=y*5+K)
  
  dt[,y:=x+2]
  
  # Set x to the mean value of b*100 in each group of a
  modify(dt,.by=c("a"), x=mean(b)*100)
  dt
  # Call with strings
  com = "x=200"
  s_modify(dt,"a==2", com)
  dt

 
}

EmptySymbol = function() (quote(f(,)))[[2]]

get.data.table.modify.call = function(args=NULL, filter.call=NULL, by=NULL, dat.quote=quote(.data)) {
  if (length(args)==1) {
    com = call(":=",names(args)[1],args[[1]])
  } else {
    return(as.expression(lapply(seq_along(args),function(i) {
      get.data.table.modify.call(args=args[i], filter.call = filter.call, by=by, dat.quote=dat.quote)
    })))
    #com = as.call(c(list(quote(`:=`)),args))
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

#' In place modification of data tables
#' 
#' If dt is a data table, then modify is essentially just a wrapper for data.table syntax that allows modification or creation of new columns. If dt is not a data table, it will by default be converted to a data table and then transformed and returned as the original data frame. Unlike mutate from dplyr, one can use the .SD argument in function calls, which can quite useful sometimes.
#'  
#' @param .dt a data.table
#' @param .if optional a boolean conditions that specifies the rows that shall be modifed
#' @param .by optional a vector of column names used for computations that are splitted by groups
#' @param ... formulas for columns that are modified or newly created
#' @param .envir optional an environment in which the expressions shall be evaluated if variables are not found in .dt
#' @param .inplace allows .dt inplace modification (TRUE if .dt is a data table)
#' @param .as.data.table shall result be a data.table (only true if .dt is a data.table)
#' @export 
modify = function(.dt,.if,.by=NULL,..., .envir=parent.frame(), .inplace=is.data.table(.dt), .as.data.table=is.data.table(.dt)) {
    
  args =  eval(substitute(alist(...)))
  if (missing(.if)) {
    filter.call=NULL
  } else {
    filter.call=substitute(.if)
  }

  if (.inplace) {
    .dt = substitute(.dt)
      
    ca = get.data.table.modify.call(args=args, by=.by, filter.call=filter.call, dat.quote=.dt)
    return(invisible(eval(ca,envir=.envir)))
  } else {
    env = new.env(parent=.envir)
    old.class = class(.dt)
    if (is.data.table(.dt)) {
      assign(".dt", copy(.dt),env)
    } else {
      assign(".dt", as.data.table(.dt),env)      
    }
    ca = get.data.table.modify.call(args=args, by=.by, filter.call=filter.call, dat.quote=quote(.dt))
    eval(ca,envir=env)
    ret = get(".dt",env)
    if (.as.data.table) {
      return(ret)
    } else {
      #try(return(as(ret, old.class)), silent=TRUE)
      return(as.data.frame(ret))
    }
  }
}

#' Modified version of modify that uses string arguments
#' @param .dt the data.table / similar object that shall be modified
#' @param ... string version of arguments of modify (see example)
#' @param .envir environment in which arguments will be evaluated (relevant if variables outside .dt are used) 
#' @export
s_modify = function(.dt, ..., .envir=parent.frame()) {
  .dt = substitute(.dt)
  data.str = paste0(deparse(.dt, width.cutoff=500), collapse="")
  args = list(...)
  args = unlist(args)
  #restore.point("s_modify")
  code = paste0("modify(",data.str,",", paste0(args, collapse=","), ", .envir=.envir)")
  invisible(eval(parse(text=code,srcfile=NULL)))
}
 