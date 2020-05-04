#' Plot the temporal trajectory in functional diversity estimated from empirical data and null models
#' @description This function plot the summary data returned by the function \code{\link{FD_nullSummary}}. This function can be called directly from \code{\link{FD_nullcom}} and from \code{\link{FD_nullsummary}}.
#' @author Simon P. Castillo \email{spcastil@@uc.cl} & Felipe Opazo-Mella
#' @param d.rao: a dataframe obtained after running the function \code{\link{FD_nullSummary}}.
#' @param d.FD: a dataframe obtained after running the function \code{\link{FD_nullSummary}}.
#' @param sumRao: a dataframe obtained after running the function \code{\link{FD_Rao}}.
#' @param sumFD: a dataframe obtained after running the function \code{\link{FD_metrics}}.
#' @param plotRao: \code{TRUE} or \code{FALSE}. Do wou want to plot the Rao's, Simpson's and functional redundancy estimates with the null models. Default \code{TRUE}.
#' @param saveRao: \code{TRUE} or \code{FALSE}. Do wou want to save the Rao's, Simpson's and functional redundancy estimates with the null models. Default \code{TRUE}.
#' @param plotFRic: \code{TRUE} or \code{FALSE}. Do wou want to plot the functional richness (FRic) estimates with the null models? Default \code{TRUE}.
#' @param saveFRic: \code{TRUE} or \code{FALSE}. Do wou want to save the Rao's, Simpson's and functional redundancy estimates with the null models. Default \code{TRUE}.
#' @return The function returns a named list with each plot made with \code{\link[ggplot2:ggplot]{ggplot2}} and  prints in the plot window (if \code{plotRao} and/or \code{plotFRic} are \code{TRUE}); and will save the corresponding plots in \code{png} format in the \code{wd}.
#' @seealso \code{\link[ggplot2:ggplot]{ggplot2}}
#' @examples FD_plotNull(d.rao = summarynullRao, d.FD = summarynullFD, plotRao = TRUE, saveRao = TRUE, plotFRic = TRUE, saveFRic = TRUE)
#'
#'
#'
FD_nullPlot <- function(d.rao, d.FD, sumRao, sumFD, plotRao=TRUE, saveRao=TRUE, plotFRic=TRUE,saveFRic=TRUE){
  pacman::p_load(ggplot2,viridis,gridExtra, grid,rlist)
  if("RaoSummary" %in% ls() == TRUE){stop("This function can be called after running the function FD_nullcom.")}
  if("FDSummary" %in% ls() == TRUE){stop("This function can be called after running the function FD_nullcom.")}
  plotsNull = list()
  if(plotRao==TRUE){
    #if("RaoSummary" %in% ls() == FALSE) {FD_rao(matrix2FDdf(data0))}
    RaoSummary <- sumRao
    n1<-ggplot(RaoSummary, aes(Time, as.numeric(as.character(Simpson)), group=1))+
      geom_point()+
      geom_line() +
      labs(x="Time", y = " Simpson index")+
      geom_line(data=d.rao, aes(x=Time, y=(mSimpson), group=1), colour="blue") +
      geom_line(data=d.rao, aes(x=Time, y=(mSimpson + Simpson.ic95), group=1), colour="blue", lty=2) +
      geom_line(data=d.rao, aes(x=Time, y=(mSimpson - Simpson.ic95), group=1), colour="blue", lty=2) +
      theme_bw()+
      theme(axis.text.x = element_text(angle=90))


    n2<-ggplot(RaoSummary, aes(Time, as.numeric(as.character(FunRao)), group=1))+
      geom_point()+
      geom_line() +
      labs(x="Time", y = " Rao index")+
      geom_line(data=d.rao, aes(x=Time, y=(mFunRao), group=1), colour="blue") +
      geom_line(data=d.rao, aes(x=Time, y=(mFunRao + FunRao.ic95), group=1), colour="blue", lty=2) +
      geom_line(data=d.rao, aes(x=Time, y=(mFunRao - FunRao.ic95), group=1), colour="blue", lty=2) +
      theme_bw()+
      theme(axis.text.x = element_text(angle=90))

    n3<-ggplot(RaoSummary, aes(Time, as.numeric(as.character(FunRedundancy)), group=1))+
      geom_point()+
      geom_line() +
      labs(x="Time", y = " Redundancy index")+
      geom_line(data=d.rao, aes(x=Time, y=(mFunRedun), group=1), colour="blue") +
      geom_line(data=d.rao, aes(x=Time, y=(mFunRedun + FunRed.ic95), group=1), colour="blue", lty=2) +
      geom_line(data=d.rao, aes(x=Time, y=(mFunRedun - FunRed.ic95), group=1), colour="blue", lty=2) +
      theme_bw()+
      theme(axis.text.x = element_text(angle=90))

    plot(n1);plot(n2);plot(n3)

    if(saveRao==TRUE){
      ggsave(plot = n1, filename = "SimpsonOverTime.png", width = 22, height = 12, units = "cm")
      ggsave(plot = n2, filename = "RaoOverTime.png", width = 22, height = 12, units = "cm")
      ggsave(plot = n3, filename = "RedundancyOverTime.png", width = 22, height = 12, units = "cm")

    }
    #return(list.append(plotsNull,simpsonplot=n1,raoplot = n2,redundancyplot=n3))

  }

  if(plotFRic==TRUE){
   #if("FDsummary" %in% ls() == FALSE) {FD_metrics(matrix2FDdf(data0))}
    FDSummary <- sumFD
  n4<-ggplot(FDSummary, aes(time, as.numeric(as.character(FRic)), group=1))+
    geom_point()+
    geom_line() +
    geom_line(data=d.FD, aes(x=Time, y=(mFRic), group=1), colour="blue") +
    geom_line(data=d.FD, aes(x=Time, y=(mFRic + FRic.ic95), group=1), colour="blue", lty=2) +
    geom_line(data=d.FD, aes(x=Time, y=(mFRic - FRic.ic95), group=1), colour="blue", lty=2) +
    labs(y="Functional Richness") +
    theme_bw()+
    theme(axis.text.x = element_text(angle=90))
  print(n4)
  #return(list.append(plotsNull, FRicplot=n4))
  }
  if(saveFRic==TRUE){
    ggsave(plot = n4, filename = "FRicOverTime.png", width = 22, height = 12, units = "cm")

  }
  #return(plotsNull)
}#ElFin


