dots = function (...) {
    eval(substitute(alist(...)))
}
named_dots = function (...) 
{
    args <- dots(...)
    nms <- names(args) 
    if (is.null(nms)) nms = rep("", length(args))
    missing <- nms == ""
    if (all(!missing)) 
        return(args)
    deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
    defaults <- vapply(args[missing], deparse2, character(1), 
        USE.NAMES = FALSE)
    names(args)[missing] <- defaults
    args
}
 
#' extended version of summarise_each that allows alternative arrangement of outputs
#' 
#' @param tbl the date frame
#' @param funs a list of functions, see the help of summarize_each, best use the helper function funs() to create them
#' @param ... columns in tbl on which the functions should be applied
#' @param .long either "funs" or "vars". Shall variables or functions be put in different rows?
#' @param .wide currently ignored
#' @.fun.var if .long = "funs" the name of the column that specifies the function
#' @.var.var if .long = "vars" the name of the column that specifies the variable
#' @export
xsummarise_each = function(tbl, funs, ..., .long="funs",.wide=NA, .fun.var = ".fun", .var.var = ".var") {
  vars = names(named_dots(...))
  fun_names = names(funs)
  if ("funs" %in% .long & !"vars" %in% .long) {
    i = 1
    li = lapply(seq_along(funs), function(i,...) {
      fun = as.character(funs[i][[1]])[1]
      dt = summarise_each(tbl, funs_(funs[i]), ...)
      cols = (NCOL(dt)-length(vars)+1):(NCOL(dt))
      colnames(dt)[cols] = vars
      cbind(.fun=fun_names[i], dt)
    },...)
    res = do.call("rbind",li)
    colnames(res)[1] = .fun.var
    return(res)

    return(do.call("rbind",li))
  } else if ("vars" %in% .long & !"funs" %in% .long) {
    qvars  = dots(...)
    li = lapply(seq_along(qvars), function(i) {
      dt = summarise_each_(tbl, funs, qvars[[i]])
      cols = (NCOL(dt)-length(fun_names)+1):(NCOL(dt))
      colnames(dt)[cols] = fun_names
      cbind(.var=vars[i], dt)
    })
    res = do.call("rbind",li)
    colnames(res)[1] = .var.var
    return(res)
  }
    
  return(summarise_each(tbl,funs,...))
}



#' extended version of summarise_each_q that allows alternative arrangement of outputs
#' 
#' @param tbl the date frame
#' @param funs a list of functions, see the help of summarize_each, best use the helper function funs() to create them
#' @param vars columns in tbl on which the functions should be applied
#' @param .long either "funs" or "vars". Shall variables or functions be put in different rows?
#' @param .wide currently ignored
#' @.fun.var if .long = "funs" the name of the column that specifies the function
#' @.var.var if .long = "vars" the name of the column that specifies the variable
#' @export
xsummarise_each_ = xsummarise_each_q = function(tbl, funs, vars, .long="funs",.wide=NA, fun.var = ".fun", var.var = ".var") {
  fun_names = names(funs)
  if ("funs" %in% .long & !"vars" %in% .long) {
    i = 1
    li = lapply(seq_along(funs), function(i) {
      fun = as.character(funs[i][[1]])[1]
      dt = summarise_each_(tbl, funs_(funs[i]), vars)
      cols = (NCOL(dt)-length(vars)+1):(NCOL(dt))
      colnames(dt)[cols] = vars
      cbind(.fun=fun_names[i], dt)
    })
    res = do.call("rbind",li)
    colnames(res)[1] = .fun.var
    return(res)

    return(do.call("rbind",li))
  } else if ("vars" %in% .long & !"funs" %in% .long) {
    li = lapply(seq_along(qvars), function(i) {
      dt = summarise_each_(tbl, funs, vars[[i]])
      cols = (NCOL(dt)-length(fun_names)+1):(NCOL(dt))
      colnames(dt)[cols] = fun_names
      cbind(.var=vars[i], dt)
    })
    res = do.call("rbind",li)
    colnames(res)[1] = .var.var
    return(res)
  }
    
  return(summarise_each_(tbl,funs,vars=vars))
}



examples.xsummarise_each = function() {
  library(dplyr)
  library(dplyrExtras)
  by_species <- group_by(iris,Species)
  summarise_each(by_species,funs(min=min, max=max, mean=mean), Petal.Width, Sepal.Width)
  xsummarise_each(by_species,funs(min=min, max=max(.,na.rm=TRUE)), Petal.Width, Sepal.Width, .long="vars", .var.var = "Variable")
  xsummarise_each(by_species,funs(min=min, max=max), Petal.Width, Sepal.Width, .long="funs")
  
  xsummarise_each_(by_species,funs(min, max), vars=c("Petal.Width", "Sepal.Width"))
  xsummarise_each_(by_species,funs(min, max), vars=c("Petal.Width", "Sepal.Width"), .long="vars", .var.var = "Variable")
}

