% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xsummarise_each.r
\name{xsummarise_each_}
\alias{xsummarise_each_}
\title{extended version of summarise_each_q that allows alternative arrangement of outputs}
\usage{
xsummarise_each_(tbl, funs, vars, .long = "funs", .wide = NA,
  fun.var = ".fun", var.var = ".var")
}
\arguments{
\item{tbl}{the date frame}

\item{funs}{a list of functions, see the help of summarize_each, best use the helper function funs() to create them}

\item{vars}{columns in tbl on which the functions should be applied}

\item{.long}{either "funs" or "vars". Shall variables or functions be put in different rows?}

\item{.wide}{currently ignored}
}
\description{
extended version of summarise_each_q that allows alternative arrangement of outputs
}

