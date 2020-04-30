#' Plot functional the functional space form empirical data
#' @param
#'      df: a data.frame obtained after running the function FD_metrics.
#'      plot.window: TRUE or FALSE if the plots will appear in the Rstudio plot window.Default TRUE.
#'      save.plot: TRUE or FALSE.Default TRUE.
#'      ext: extension of the plot to be saved. Only extenssion admitted by ggsave (e.g., "svg"). Default ".png".
#' @return
#'      if plot.window is TRUE the plot will appear in the IDE plot window. If save.plot is TRUE plots will be saved as png in your wd.



FD_plotPC <- function(df, plot.window=TRUE,save.plot= TRUE, ext=".png"){
  print("Warning: this function depends on the outputs of FD_metrics and FD_rao functions")
  pacman::p_load(ggplot2,viridis,gridExtra, grid)
  plot.time=unique(df$time)

  for (i in 1:length(plot.time)) {
    plotit<-data.frame(id=data1$ecocode[which(data1$time == plot.time[i])], abplot= data1$abundance[which(data1$time == plot.time[i])])
    ab<-plotit$abplot[-which(plotit$abplot == 0)]
    colPoints <-rainbow(n = length(ab))
    PCSname <- paste0("PCS_",plot.time[i])
    dataPCS<- FuncSpace[[i]]
    dataconhull1 <-  ch1_list[[i]]
    dataconhull2 <-  ch2_list[[i]]
    pc1<-pc1_list[[i]]
    pc2<-pc2_list[[i]]
    pc3<-pc3_list[[i]]

    p<-ggplot(data=dataPCS, aes(x=PC1, y= PC2 ))  +
      geom_polygon(data = dataconhull1, aes( x= GPC1, y=GPC2), alpha = 0.6, fill=rgb(.7,.8,.9,0.2)) +  #convex hull
      geom_point(shape=21, size=ab*100, fill= colPoints, colour="black", alpha=.75) + #points
      geom_point(data=dataPCS, aes(x=mean(PC1), y=mean(PC2)), shape=3, size=4, colour = rgb(1, 0, 0)) + #Mean point (red + at the middle)
      # geom_text_repel(aes(label = rownames(dataPCS)), size = 4, col = colPoints, nudge_y = -0.3, nudge_x = -.1, segment.colour = "black") +  #labels
      scale_colour_manual("black") +
      labs( x=paste("PC1 ",pc1*100, "%", sep =""), y=paste("PC2 ",pc2*100, "%", sep =""),
            title= paste(paste0("FRic=", round(as.numeric(as.character(FDsummary$FRic[i])), 2)),paste0("FRed=", round(as.numeric(as.character(RaoSummary$FunRedundancy[i])),2)), sep= " ")) +
      theme_bw() +
      theme(axis.title.x=element_text(size=15), axis.title.y = element_text(size=15),panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    q <-ggplot(data=dataPCS, aes(x=PC1, y= PC3 ))  +
      geom_polygon(data = dataconhull2, aes( x= GPC1, y=GPC3), alpha = 0.6, fill=rgb(.7,.8,.9,0.2)) +  #convex hull
      geom_point(shape=21, size=ab*100, fill= colPoints, colour="black", alpha=.75) + #points
      geom_point(data=dataPCS, aes(x=mean(PC1), y=mean(PC3)), shape=3, size=4, colour = rgb(1, 0, 0)) + #Mean point (red + at the middle)
      #geom_text_repel(aes(label = rownames(dataPCS)), size = 4, col = colPoints, nudge_y = -0.3, nudge_x = -.1, segment.colour = "black") +  #labels
      scale_colour_manual("black") +
      labs( x=paste("PC1 ",pc1*100, "%", sep =""), y=paste("PC3 ",pc3*100, "%", sep =""),
            title="")+# paste(paste0("FRic=", round(as.numeric(as.character(FDmetrics$FRic[i])), 2)),paste0("FRed=", round(as.numeric(as.character(RaoSummary$FunRedundancy[i])),2)), sep= " ")) +
      theme_bw() +
      theme(axis.title.x=element_text(size=15), axis.title.y = element_text(size=15),panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())


    r=grid.arrange(p, q, ncol=2, nrow=1, top = textGrob(paste0("Date: ",as.character(plot.time[i])),gp=gpar(fontsize=20,font=1)))
    if(plot.window== TRUE) {print(r)}
    if(save.plot== TRUE) {ggsave(plot = r, filename = paste0("plotFunctionalSpace_", plot.time[i],ext), height = 14, width = 20, units = "cm")}
  }
}#fin
