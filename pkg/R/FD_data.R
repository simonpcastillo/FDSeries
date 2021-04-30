#' Internal datasets
#'
#' @author Simon P. Castillo \email{spcastil@@uc.cl}, Felipe Opazo-Mella, Fabio Labra and Pablo Marquet
#' @description Two dataset corresponding to the two types of input used in this package. The data0 dataframe contains the taxonomic records and their corresponding ecological features. The data1 dataframe contains the occurrence estimated for each ecocode for each time. These data frames, data0 and data1, can be used in \code{\link{FD_rarefaction}} or \code{\link{FD_df}}, respectively. See the arguments of each function.
#' @docType data
#' @name FDSeries-Datasets
#' @keywords datasets
#' @examples View(data0)
#' @examples ncol(data1) #Number of time bins
#' @examples nrow(data1) #Number of life-modes or ecocodes
#' @examples FD_rarefaction(df=data0, times=500)
#' @examples FD_df(df=data1, subcat=c(6,6,6))
#'
#'
NULL
