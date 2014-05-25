\name{modify}
\alias{modify}
\title{Fast in place modification of data.frames and data.tables}
\usage{
modify(.data, .if, .by = NULL, ..., .envir = parent.frame())
}
\arguments{
  \item{.data}{a data.frame, data.table or dplyr tbl
  object}

  \item{.if}{optional a boolean conditions that specifies
  the rows that shall be modifed}

  \item{.by}{optional a vector of column names used for
  computations that are splitted by groups}

  \item{...}{formulas for columns that are modified or
  newly created}
}
\description{
modify is essentially just a wrapper for data.table syntax
but it can be used for other data containers. It is thought
as an addition to dplyr functions. While the functionality
can be replicated by mutate, modify can be much faster and
more concise if only values for selected rows shall be
modified.
}

\examples{\dontrun{
  
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
  


}}
