#' Plot the functional space form empirical data
#' @description Plot the functional space for each time bin as paired coordinates PC1 vs PC2 and PC1 vs PC3.
#' @author Simon P. Castillo \email{spcastil@@uc.cl}, Felipe Opazo-Mella, Fabio Labra and Pablo Marquet
#' @param dfFD: dataframe. A functional diversity dataframe created with the function \code{\link{FD_df}}.
#' @param sumRao: a dataframe obtained after running the function \code{\link{FD_Rao}}.
#' @param sumFD: a dataframe obtained after running the function \code{\link{FD_metrics}}.
#' @param plot.window: \code{TRUE} or \code{FALSE} if the plots will appear in the Rstudio plot window.Default \code{TRUE}.
#' @param save.plot: \code{TRUE} or \code{FALSE}. Default \code{TRUE}.
#' @param add.labs: \code{TRUE} or \code{FALSE}. It adds the label corresponding to each life-mode or ecocode. Default \code{FALSE}.
#' @param ext: extension of the plot to be saved. Only extenssion admitted by \code{\link[ggplot2]{ggsave}} (e.g., \code{".svg"}, \code{".png"}). Default \code{".png"}.
#' @return The function returns a list with a plot for each time bin. If \code{plot.window} is \code{TRUE} the plot will appear in the IDE plot window. If \code{save.plot} is \code{TRUE} plots will be saved with a \code{".png"} extension in your \code{wd}.
#' @examples View(FDsummary)
#'
#' @examples FD_plotFS(FDsummary, plot.window=TRUE,save.plot= TRUE,add.labs=FALSE, ext=".png" )
#'
#' plotsFS <- FD_plotFS(FDsummary, plot.window=TRUE,save.plot= TRUE,add.labs=FALSE, ext=".png" ) #
#' #plotsFS is a list
#'
#' @seealso \code{\link{FD_metrics}}, \code{\link[ggplot2]{ggsave}}.
#'
#'
#'
FD_plotFS <- function(dfFD, sumFD, sumRao, plot.window=TRUE,cols = viridis, save.plot= TRUE,add.labs=FALSE, ext=".png"){
  #warning("This function depends on the outputs of FD_metrics and FD_Rao functions")
  pacman::p_load(ggplot2,viridis,gridExtra, grid, ggrepel, patchwork)
  plot.time=unique(dfFD$time)
  plotsFS <- list()

  for (i in 1:length(plot.time)) {
    plotit<-data.frame(id=dfFD$ecocode[which(dfFD$time == as.character(plot.time[i]))], abplot= dfFD$abundance[which(dfFD$time == plot.time[i])])
    ab<-plotit$abplot[-which(plotit$abplot == 0)]
    colPoints <-cols(n = length(ab))
    PCSname <- paste0("PCS_",plot.time[i])
    dataPCS<- FuncSpace[[i]]
    dataconhull1 <-  ch1_list[[i]]
    dataconhull2 <-  ch2_list[[i]]
    pc1<-pc1_list[[i]]
    pc2<-pc2_list[[i]]
    pc3<-pc3_list[[i]]
    labelsB <- rownames(dataPCS)
    dataPCS$labels= rownames(dataPCS)
    if(add.labs == FALSE){
    p <- ggplot(data=dataPCS, aes(x=PC1, y= PC2)) +
      geom_polygon(data = dataconhull1, aes( x= GPC1, y=GPC2), alpha = 0.6, fill=rgb(.7,.8,.9,0.2)) +  #convex hull
      geom_point(shape=21, size=ab*100, fill= colPoints, colour="black", alpha=.75) + #points
      geom_point(data=dataPCS, aes(x=mean(PC1), y=mean(PC2)), shape=3, size=4, colour = rgb(1, 0, 0)) + #Mean point (red + at the middle)
      scale_colour_manual("black") +
      labs( x=paste("PC1 ",pc1*100, "%", sep =""), y=paste("PC2 ",pc2*100, "%", sep =""),
            title= paste(paste0("FRic=", round(as.numeric(as.character(sumFD$FRic[i])), 2)),paste0("FRed=", round(as.numeric(as.character(sumRao$FunRedundancy[i])),2)), sep= " ")) +
      theme_bw() +
      theme(axis.title.x=element_text(size=15), axis.title.y = element_text(size=15),panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    q <- ggplot(data=dataPCS, aes(x=PC1, y= PC3)) +
      geom_polygon(data = dataconhull2, aes( x= GPC1, y=GPC3), alpha = 0.6, fill=rgb(.7,.8,.9,0.2)) +  #convex hull
      geom_point(shape=21, size=ab*100, fill= colPoints, colour="black", alpha=.75) + #points
      geom_point(data=dataPCS, aes(x=mean(PC1), y=mean(PC3)), shape=3, size=4, colour = rgb(1, 0, 0)) + #Mean point (red + at the middle)
      scale_colour_manual("black") +
      labs( x=paste("PC1 ",pc1*100, "%", sep =""), y=paste("PC3 ",pc3*100, "%", sep =""),
            title="")+
      theme_bw() +
      theme(axis.title.x=element_text(size=15), axis.title.y = element_text(size=15),panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    r <- p+q+plot_annotation(title = paste0("Date: ",as.character(plot.time[i])))
    plotsFS[[i]] <- r}

    if(add.labs == TRUE){
          p <- ggplot(data=NULL) +
            geom_polygon(data = dataconhull1, aes( x= GPC1, y=GPC2), alpha = 0.6, fill=rgb(.7,.8,.9,0.2)) +  #convex hull
            geom_point(data=dataPCS,aes(x=PC1, y= PC2),shape=21, size=ab*100, fill= colPoints, colour="black", alpha=.75) +
            geom_point(data=dataPCS, aes(x=mean(PC1), y=mean(PC2)), shape=3, size=4, colour = rgb(1, 0, 0)) + #Mean point (red + at the middle)
            scale_colour_manual("black") +
            geom_text_repel(data=dataPCS, aes(x=PC1, y= PC2,label = labels), size = 4, col = "black", nudge_y = -0.3, nudge_x = -.1, segment.colour = "black") + #points
            labs( x=paste("PC1 ",pc1*100, "%", sep =""), y=paste("PC2 ",pc2*100, "%", sep =""),
                  title= paste(paste0("FRic=", round(as.numeric(as.character(sumFD$FRic[i])), 2)),paste0("FRed=", round(as.numeric(as.character(sumRao$FunRedundancy[i])),2)), sep= " ")) +
            theme_bw() +
            theme(axis.title.x=element_text(size=15), axis.title.y = element_text(size=15),panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())

          q <- ggplot(data=NULL) +
            geom_polygon(data = dataconhull2, aes( x= GPC1, y=GPC3), alpha = 0.6, fill=rgb(.7,.8,.9,0.2)) +  #convex hull
            geom_point(data=dataPCS,aes(x=PC1, y= PC3),shape=21, size=ab*100, fill= colPoints, colour="black", alpha=.75) + #point
            geom_text_repel(data=dataPCS, aes(x=PC1, y= PC3,label = labels), size = 4, col = "black", nudge_y = -0.3, nudge_x = -.1, segment.colour = "black")+
            geom_point(data=dataPCS, aes(x=mean(PC1), y=mean(PC3)), shape=3, size=4, colour = rgb(1, 0, 0)) + #Mean point (red + at the middle)
            scale_colour_manual("black") +
            labs( x=paste("PC1 ",pc1*100, "%", sep =""), y=paste("PC3 ",pc3*100, "%", sep =""),
                  title="")+
            theme_bw() +
            theme(axis.title.x=element_text(size=15), axis.title.y = element_text(size=15),panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
          r <- p+q+plot_annotation(title = paste0("Date: ",as.character(plot.time[i])))
          plotsFS[[i]] <- r


      }

    #r=grid.arrange(p, q, ncol=2, nrow=1, top = textGrob(paste0("Date: ",as.character(plot.time[i])),gp=gpar(fontsize=20,font=1)))
    r <- p+q+plot_annotation(title = paste0("Date: ",as.character(plot.time[i])))
    if(plot.window == TRUE) {plot(r)}
    if(save.plot == TRUE) {ggsave(plot = r, filename = paste0("plotFunctionalSpace_", plot.time[i],ext), height = 14, width = 20, units = "cm")}

  }
  plotsFS <<- plotsFS
 # return(plotsFS)
}#ElFin
