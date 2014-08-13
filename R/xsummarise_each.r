 
#' extended version of summarise_each that allows alternative arrangement of outputs
#' 
#' currently it is only checked whether "funs" is in .long or not. if yes
#' then functions will be shown in different rows
#' 
#' @export
xsummarise_each = function(tbl, funs, ..., .long="funs",.wide="vars") {
  if ("funs" %in% .long) {
    i = 1
    li = lapply(seq_along(funs), function(i,...) {
      fun = as.character(funs[i][[1]])[1]
      dt = summarise_each(tbl, funs_q(funs[i]), ...)
      cbind(.fun=fun, dt)
    },...)
    return(do.call("rbind",li))
  }
  return(summarise_each(tbl,funs,...))
}

#' extended version of summarise_each_q that allows alternative arrangement of outputs
#' 
#' currently it is only checked whether "funs" is in .long or not.
#' 
#' @export
xsummarise_each_q = function(tbl, funs, vars, .long="funs",.wide="vars") {
  if ("funs" %in% .long) {
    i = 1
    li = lapply(seq_along(funs), function(i) {
      fun = as.character(funs[i][[1]])[1]
      dt = summarise_each_q(tbl, funs=funs_q(funs[i]), vars=vars)
      cbind(.fun=fun, dt)
    })
    return(do.call("rbind",li))
  }
  return(summarise_each_q(tbl,funs,vars=vars))
}


examples.xsummarise_each = function() {
  library(dplyr)
  library(dplyrExtras)
  by_species <- group_by(iris,Species)
  by_species %>% summarise_each(funs(min, max), Petal.Width, Sepal.Width)
  by_species %>% xsummarise_each(funs(min, max), Petal.Width, Sepal.Width)
  
  xsummarise_each_q(by_species,funs(min, max), vars=c("Petal.Width", "Sepal.Width"))

  by_species %>% xsummarise_each_q(funs(min, max), vars=c("Petal.Width", "Sepal.Width"))
}


examples.xsummarise_each = function() {
  library(dplyr)
  library(dplyrExtras)
  by_species <- group_by(iris,Species)
  summarise_each(by_species,funs(min, max), Petal.Width, Sepal.Width)
  xsummarise_each(by_species,funs(min, max), Petal.Width, Sepal.Width)
  xsummarise_each_q(by_species,funs(min, max), vars=c("Petal.Width", "Sepal.Width"))
}