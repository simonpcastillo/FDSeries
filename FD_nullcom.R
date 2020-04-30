#' Simmulate random communities
#' @param
#'      df: input data.frame with rows indicating time bins and columns indicating each life-mode or ecocode. The df values are estimates of abundance.
#'      nNull: numeric. Number of null replicates to compute.
#'      call.nullsummary: TRUE or FALSE. Do you want to call the FD_nullsummary function of this package an summarise the output? Default FALSE.
#'      plot.null: TRUE or FALSE. It works ONLY if call.nullsumamry is TRUE and pass arguments to the function FD_nullSummary.
#' @return data.frame


FD_nullcom <- function(df, nNull= 10, call.nullsummary=FALSE, plot.null=FALSE){
  pacman::p_load(plyr)
  null_rao= data.frame()
  null_FDmetrics <- data.frame()
  for (l in 1:nNull) {
    data_n <- nullmodel_FD(data0,type="all")
    data1n <- FD_df(data_n)
    summaryn1 <- FD_rao(data1n); print("<Summary1_RaoFD.csv_rep> has been done!")
    summaryn2 <- FD_metrics(data1n); print("<Summary2_FDmetrics.csv_rep_> has been created!")
    summaryn1$nullcomm<- summaryn2$nullcomm <- l
    null_rao<-rbind(null_rao,summaryn1)
    null_FDmetrics<-rbind(null_FDmetrics,summaryn2)
    print(l)
  }

  null_rao<<- null_rao
  null_FDmetrics <<- null_FDmetrics

  if(call.nullsummary==TRUE && plot.null==TRUE){
    FD_nullSummary(nullrao = null_rao, nullFD = null_FDmetrics, nNull = nNull, plot.null = TRUE)
  }

  if(call.nullsummary==TRUE && plot.null==FALSE){
    FD_nullSummary(nullrao = null_rao, nullFD = null_FDmetrics, nNull = nNull, plot.null = FALSE)
  }


}
