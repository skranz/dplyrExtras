# Helper functions that allow string arguments for dplyr's data modification functions like arrange, select etc.
# Author: Sebastian Kranz
 
# Examples are below

 
#' Modified version of dplyr's filter that uses string arguments
#' @param .data the data frame (or similar object)
#' @param ... string version of arguments to original dplyr function
#' @export
s_filter = function(.data, ...) {
eval.string.dplyr(.data,"filter", ...)
}
 
#' Modified version of dplyr's select that uses string arguments
#' @param .data the data frame (or similar object)
#' @param ... string version of arguments to original dplyr function
#' @export
s_select = function(.data, ...) {
eval.string.dplyr(.data,"select", ...)
}
 
#' Modified version of dplyr's arrange that uses string arguments
#' @param .data the data frame (or similar object)
#' @param ... string version of arguments to original dplyr function
#' @export
s_arrange = function(.data, ...) {
eval.string.dplyr(.data,"arrange", ...)
}
 
#' Modified version of dplyr's mutate that uses string arguments
#' @param .data the data frame (or similar object)
#' @param ... string version of arguments to original dplyr function
#' @export
s_mutate = function(.data, ...) {
eval.string.dplyr(.data,"mutate", ...)
}
 
#' Modified version of mutate_if that uses string arguments
#' @param .data the data frame (or similar object)
#' @param ... string version of arguments to mutate_if
#' @export
s_mutate_if = function(.data, ...) {
eval.string.dplyr(.data,"mutate_if", ...)
}
 

#' Modified version of summarise that uses string arguments
#' @param .data the data frame (or similar object)
#' @param ... string version of arguments to original dplyr function
#' @export
s_summarise = function(.data, ...) {
eval.string.dplyr(.data,"summarise", ...)
}
 
#' Modified version of dplyr's group_by that uses string arguments
#' @param .data the data frame (or similar object)
#' @param ... string version of arguments to original dplyr function
#' @export
s_group_by = function(.data, ...) {
eval.string.dplyr(.data,"group_by", ...)
}
 
# Internal function used by s_filter, s_select etc.
eval.string.dplyr = function(.data, .fun.name, ...) {
  args = list(...)
  args = unlist(args)
  code = paste0(.fun.name,"(.data,", paste0(args, collapse=","), ")")
  df = eval(parse(text=code,srcfile=NULL))
  df
}

examples.s_filter = examples.s_arrange = examples.s_select = examples.s_mutate = examples.s_summarise = examples.s_mutate_if = function() {
  library(dplyrExtras)
  
  d = mtcars
  cols = c("mpg","cyl","hp:vs")
  s_select(d,cols)
  # also works and yields identical result...
  cols = c("mpg","cyl, hp:vs")
  s_select(d,cols)
  
  s_filter(d,"gear == 3","cyl == 8")
  s_arrange(d, "-mpg, gear, carb")
  gd = s_group_by(d,"cyl")
  s_summarise(gd, "mean(disp), max(disp)")
  s_mutate_if(d, "cyl==6, new.col=100")
  
}

#' A variant of dplyr::recode which allows to provide
#' old and new values as vectors, instead of named arguments
#' 
#' @param x a vector of values
#' @param new a vector of replacement values. 
#'            If new is named and old is not provided, the names
#'            of new are the old values to be replaced
#' @param old a vector old values to be replaced by new
#' @param .default like in recode
#' @param .missing like in recode
#' @export

recode_ = function(x, new, old=names(new), .default=NULL, .missing=NULL) {
  if (is.character(old)) {
    rows = match(as.character(x),old)
  } else {
    rows = match(x,old)
  }
  has.match = !is.na(rows)
  x[has.match] = new[rows[has.match]]
  na.rows = c()
  if (!is.null(.missing)) {
    na.rows = is.na(x)
    x[na.rows] = .missing
  }
  if (!is.null(.default)) {
    x[!has.match & !na.rows] = .default
  }
  x
}
