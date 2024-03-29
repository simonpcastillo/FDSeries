% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FD_nullCom.R
\name{FD_nullCom}
\alias{FD_nullCom}
\title{Simmulate random communities}
\usage{
FD_nullCom(
  df,
  nNull = 10,
  subcat,
  type = "all",
  call.nullsummary = FALSE,
  plot.null = FALSE
)
}
\arguments{
\item{df:}{input dataframe with rows indicating time bins and columns indicating each life-mode or ecocode. The \code{df} values are estimates of abundance. This is your original dataframe, the same used by \code{\link{DF_fd}}.}

\item{nNull:}{numeric. Number of null replicates (communities) to compute. Default 10.}

\item{subcat:}{numeric vector. The number of subcategories for each trait.Its length must be equal that the number of traits of each life-mode.It must be the same that in \code{\link{FD_df}}}

\item{type:}{type of randomisation. It allows one of the following options Default \code{"all"}, Default \code{"col-wise"}, Default \code{"row-wise"}. Default \code{"all"}.}

\item{call.nullsummary:}{\code{TRUE} or \code{FALSE}. Do you want to call the \code{\link{FD_nullsummary}} function of this package and summarise the output? Default \code{FALSE}.}

\item{plot.null:}{\code{TRUE} or \code{FALSE}. It works ONLY if \code{call.nullsumamry} is \code{TRUE} and pass arguments to the function \code{\link{FD_nullSummary}}.}
}
\value{
The function returns to the Global Environment two dataframes \code{null_Rao} and \code{null_FDmetrics} with the estimated functional diversity metrics for each replicate calculated by the function \code{\link{FD_rao}} and \code{\link{FD_metrics}}.
If \code{call.nullsummary} is \code{TRUE}, the function \code{\link{FD_nullsummary}} is called internally and returns to the environment two dataframes \code{summarynullrao} and \code{summarynullFD}.
Finally, if \code{plot.null} is \code{TRUE}, the function \code{\link{FD_plotnull}} is called internally returning the respective plots.
}
\description{
This function simmulates random communities from a resampling of the abundances in the empirical data.
}
\examples{
View(data1) # original dataframe, it is not a functional diversity dataframe

FD_nullcom(data1, nNull= 10, type= "all", call.nullsummary = FALSE, plot.null = FALSE)) # Note: in this case it doesn't summarise nor plot the data.



}
\author{
Simon P. Castillo \email{spcastil@uc.cl}, Felipe Opazo-Mella, Fabio Labra and Pablo Marquet
}
