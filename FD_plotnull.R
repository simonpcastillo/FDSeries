#' Plot the temporal trajectory in functional diversity estimated from empirical data and null models
#' @param
#'      d.rao: a data.frame obtained after running the function FD_nullcom.
#'      d.FD: a data.frame obtained after running the function FD_nullcom.
#'      plotRao: TRUE or FALSE. Do wou want to plot the Rao's, Simpson's and functional redundancy estimates with the null models. Default TRUE.
#'      saveRao: TRUE or FALSE. Do wou want to save the Rao's, Simpson's and functional redundancy estimates with the null models. Default TRUE.
#'      plotFRic: TRUE or FALSE. Do wou want to plot the functional richness (FRic) estimates with the null models. Default TRUE.
#'      saveFRic: TRUE or FALSE. Do wou want to save the Rao's, Simpson's and functional redundancy estimates with the null models. Default TRUE.

#' @return The functions returns ggplots in the plot window (if plotRao and/or plotFRics are TRUE); and will save the corresponding plots in png formatin the working directory.


FD_plotNull <- function(d.rao, d.FD, plotRao=TRUE,saveRao=TRUE, plotFRic=TRUE,saveFRic=TRUE){
  pacman::p_load(ggplot2,viridis,gridExtra, grid)
  if("RaoSummary" %in% ls() == TRUE){stop("This function can be called after running the function FD_nullcom.")}
  if("FDsummary" %in% ls() == TRUE){stop("This function can be called after running the function FD_nullcom.")}
  if(plotRao==TRUE){
    #if("RaoSummary" %in% ls() == FALSE) {FD_rao(matrix2FDdf(data0))}

    n1<-ggplot(RaoSummary, aes(Time, as.numeric(as.character(Simpson)), group=1))+
      geom_point()+
      geom_line() +
      labs(x="Time", y = " Simpson index")+
      geom_line(data=d.rao, aes(x=Time, y=(mSimpson), group=1), colour="red") +
      geom_line(data=d.rao, aes(x=Time, y=(mSimpson + Simpson.ic95), group=1), colour="red", lty=2) +
      geom_line(data=d.rao, aes(x=Time, y=(mSimpson - Simpson.ic95), group=1), colour="red", lty=2) +
      theme_bw()+
      theme(axis.text.x = element_text(angle=90))


    n2<-ggplot(RaoSummary, aes(Time, as.numeric(as.character(FunRao)), group=1))+
      geom_point()+
      geom_line() +
      labs(x="Time", y = " Rao index")+
      geom_line(data=d.rao, aes(x=Time, y=(mFunRao), group=1), colour="red") +
      geom_line(data=d.rao, aes(x=Time, y=(mFunRao + FunRao.ic95), group=1), colour="red", lty=2) +
      geom_line(data=d.rao, aes(x=Time, y=(mFunRao - FunRao.ic95), group=1), colour="red", lty=2) +
      theme_bw()+
      theme(axis.text.x = element_text(angle=90))

    n3<-ggplot(RaoSummary, aes(Time, as.numeric(as.character(FunRedundancy)), group=1))+
      geom_point()+
      geom_line() +
      labs(x="Time", y = " Redundancy index")+
      geom_line(data=d.rao, aes(x=Time, y=(mFunRedun), group=1), colour="red") +
      geom_line(data=d.rao, aes(x=Time, y=(mFunRedun + FunRed.ic95), group=1), colour="red", lty=2) +
      geom_line(data=d.rao, aes(x=Time, y=(mFunRedun - FunRed.ic95), group=1), colour="red", lty=2) +
      theme_bw()+
      theme(axis.text.x = element_text(angle=90))
    print(n1)
    print(n2)
    print(n3)

    if(saveRao==TRUE){
      ggsave(plot = n1, filename = "SimpsonOverTime.png", width = 22, height = 12, units = "cm")
      ggsave(plot = n2, filename = "RaoOverTime.png", width = 22, height = 12, units = "cm")
      ggsave(plot = n3, filename = "RedundancyOverTime.png", width = 22, height = 12, units = "cm")

    }
  }

  if(plotFRic==TRUE){
   #if("FDsummary" %in% ls() == FALSE) {FD_metrics(matrix2FDdf(data0))}
  n4<-ggplot(FDsummary, aes(time, as.numeric(as.character(FRic)), group=1))+
    geom_point()+
    geom_line() +
    geom_line(data=d.FD, aes(x=Time, y=(mFRic), group=1), colour="red") +
    geom_line(data=d.FD, aes(x=Time, y=(mFRic + FRic.ic95), group=1), colour="red", lty=2) +
    geom_line(data=d.FD, aes(x=Time, y=(mFRic - FRic.ic95), group=1), colour="red", lty=2) +
    labs(y="Functional Richness") +
    theme_bw()+
    theme(axis.text.x = element_text(angle=90))
  print(n4)

  }
  if(saveFRic==TRUE){
    ggsave(plot = n4, filename = "FRicOverTime.png", width = 22, height = 12, units = "cm")
    }
}


