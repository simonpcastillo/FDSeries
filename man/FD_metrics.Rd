% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FD_metrics.R
\name{FD_metrics}
\alias{FD_metrics}
\title{Estimate functional metrics}
\source{
This code is an edited version of Villeger: \url{http://villeger.sebastien.free.fr/Rscripts.html}.
}
\usage{
FD_metrics(df)
}
\arguments{
\item{df:}{a dataframe obtained after running the \code{\link{FD_df}} function.}
}
\value{
FDsummary: \code{dataframe}. It contains the estimated parameters for functional diversity for each time bin. \emph{FEve} functional evenness, \emph{FDiv} functional divergence, \emph{FSpe} functional specialisaiton, \emph{FRic} functional richness.

FuncSpace: list to the Global Environment. Stores the Functional space for each time bin.

ch1_list: list to the Global Environment. Stores convex hull coordinates.Graphical use in \code{\link{FD_plotPC}}

ch2_list: list to the Global Environment. Stores convex hull coordinates.Graphical use in \code{\link{FD_plotPC}}

pc1_list: list to the Global Environment. Stores principal component coordinates.Graphical use in \code{\link{FD_plotPC}}

pc2_list: list to the Global Environment. Stores principal component coordinates.Graphical use in \code{\link{FD_plotPC}}

pc3_list: list to the Global Environment. Stores principal component coordinates.Graphical use in \code{\link{FD_plotPC}}
}
\description{
This function is based on Villeger et al. (2008). This function computes functional diversity metrics.
}
\examples{
FDSummary <- FD_metrics(df=data1)

View(FDsummary)



}
\references{
Villéger, S., Mason, N. W., & Mouillot, D. (2008). New multidimensional functional diversity indices for a multifaceted framework in functional ecology. Ecology, 89(8), 2290-2301.
}
\seealso{
\code{\link{FD_df}}, \code{\link[FD:FD-package]{FD}}
}
\author{
Simon P. Castillo \email{spcastil@uc.cl} & Felipe Opazo-Mella
}
