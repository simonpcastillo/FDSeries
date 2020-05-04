#' Simmulate random communities
#' @author Simon P. Castillo \email{spcastil@@uc.cl} & Felipe Opazo-Mella
#' @description This function simmulates random communities from a resampling of the abundances in the empirical data.
#' @param df: input dataframe with rows indicating time bins and columns indicating each life-mode or ecocode. The \code{df} values are estimates of abundance. This is your original dataframe, the same used by \code{\link{DF_fd}}.
#' @param nNull: numeric. Number of null replicates (communities) to compute. Default 10.
#' @param subcat: numeric vector. The number of subcategories for each trait.Its length must be equal that the number of traits of each life-mode.It must be the same that in \code{\link{FD_df}}
#' @param type: type of randomisation. It allows one of the following options Default \code{"all"}, Default \code{"col-wise"}, Default \code{"row-wise"}. Default \code{"all"}.
#' @param call.nullsummary: \code{TRUE} or \code{FALSE}. Do you want to call the \code{\link{FD_nullsummary}} function of this package and summarise the output? Default \code{FALSE}.
#' @param plot.null: \code{TRUE} or \code{FALSE}. It works ONLY if \code{call.nullsumamry} is \code{TRUE} and pass arguments to the function \code{\link{FD_nullSummary}}.
#' @return The function returns to the Global Environment two dataframes \code{null_Rao} and \code{null_FDmetrics} with the estimated functional diversity metrics for each replicate calculated by the function \code{\link{FD_rao}} and \code{\link{FD_metrics}}.
#' If \code{call.nullsummary} is \code{TRUE}, the function \code{\link{FD_nullsummary}} is called internally and returns to the environment two dataframes \code{summarynullrao} and \code{summarynullFD}.
#' Finally, if \code{plot.null} is \code{TRUE}, the function \code{\link{FD_plotnull}} is called internally returning the respective plots.
#'
#' @examples View(data0) # original dataframe, it is not a functional diversity dataframe
#'
#' @examples FD_nullcom(data0, nNull= 10, type= "all", call.nullsummary = FALSE, plot.null = FALSE)) # Note: in this case it doesn't summarise nor plot the data.
#'
#'
#'
FD_nullCom <- function(df, nNull= 10,subcat ,type= "all", call.nullsummary=FALSE, plot.null=FALSE){
  if(plot.null == TRUE & call.nullsummary==FALSE)stop("if plot.null = TRUE, call.nullsummary MUST be TRUE")
  pacman::p_load(plyr)
  null_rao= data.frame()
  null_FDmetrics <- data.frame()
  for (l in 1:nNull) {
    data_n <- nullmodel_FD(df,type=type)
    data1n <- FD_df(data_n, subcat = subcat)
    summaryn1 <- FD_Rao(data1n)
    summaryn2 <- FD_metrics(data1n)
    summaryn1$nullcomm<- summaryn2$nullcomm <- l
    null_rao<-rbind(null_rao,summaryn1)
    null_FDmetrics<-rbind(null_FDmetrics,summaryn2)
    #print(paste0("null replicate: ",l))
  }

  null_Rao<<- null_rao
  null_FDmetrics <<- null_FDmetrics

  if(call.nullsummary==TRUE && plot.null==TRUE){
    FD_nullSummary(nullrao = null_Rao, nullFD = null_FDmetrics, nNull = nNull, plot.null = TRUE)
  }

  if(call.nullsummary==TRUE && plot.null==FALSE){
    FD_nullSummary(nullrao = null_Rao, nullFD = null_FDmetrics, nNull = nNull, plot.null = FALSE)
  }


}#ElFin
