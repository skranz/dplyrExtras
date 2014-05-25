\name{s_mutate_if}
\alias{s_mutate_if}
\title{Modified version of mutate_if that uses string arguments}
\usage{
s_mutate_if(.data, ...)
}
\description{
Modified version of mutate_if that uses string arguments
}

\examples{\dontrun{
  library(dplyrExtras)
  
  d = mtcars
  cols = c("mpg","cyl, hp:vs")
  s_select(d,cols)
  s_filter(d,"gear == 3","cyl == 8")
  s_arrange(d, "-mpg, gear, carb")
  gd = s_group_by(d,"cyl")
  s_summarise(gd, "mean(disp), max(disp)")
  s_mutate_if(d, "cyl==6, new.col=100")
  
}}
