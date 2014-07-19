\name{mutate_if}
\alias{mutate_if}
\title{mutate selected rows}
\usage{
mutate_if(.data, .if, ...)
}
\arguments{
  \item{.data}{the data}

  \item{.if}{a logical condition that selects rows, e.g.
  a=="B"}

  \item{...}{the command to mutate existing columns}
}
\description{
change values of columns only in rows that satisfy the .if
condition
}

\examples{\dontrun{
  library(microbenchmark)
  library(dplyr)
  library(data.table)
  library(dplyrExtras)

  # Test mutate_if
  set.seed(123456)
  n = 10
  df = data.frame(a= sample(1:3,n,replace=TRUE),
  b= sample(1:100,n,replace=TRUE),
  x=rnorm(n))
  dt = as.data.table(df)
  mutate_if(df,a==3,x=100)
  mutate_if(tbl_df(df),a==1,x=200)
  mutate_if(as.tbl(df),a==1,x=300,b=400)
  mutate_if(dt,a==1 | a==2,x=400)
  mutate_if(group_by(dt,a),a==1 | a==2,x=mean(b)) 
  mutate_if(group_by(df,a),a==1 | a==2,x=mean(b))
  
  
  # Small benchmark: compare with mutate + ifelse
  n = 1e6
  df = data.frame(a= sample(1:3,n,replace=TRUE),
  b= sample(1:100,n,replace=TRUE),
  x=rnorm(n))
  microbenchmark(times = 5L,
    mutate(df, x=ifelse(a==2,x+100,x)),
    mutate_if(df, a==2, x=x+100),
  )
  #Unit: milliseconds
  # expr min lq median uq max neval
  # mutate(df, x = ifelse(a == 2, x + 100, x)) 749.2954 754.4179 815.06681 820.95872 860.79326 5
  # mutate_if(df, a == 2, x = x + 100) 72.2886 75.4189 77.47787 83.64689 86.33666 5
}}
