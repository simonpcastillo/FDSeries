#' Plot functional metrics over time from empirical data
#' @description Plot functional metrics (Simpson, Rao's quadratic Entropy and Funcional redundancy) over time from empirical data.
#' @author Simon P. Castillo \email{spcastil@@uc.cl} & Felipe Opazo-Mella
#' @param df: dataframe obtained after running \code{\link{FD_Rao}}.
#' @param save.plot: \code{TRUE} or \code{FALSE}. Default \code{TRUE}.
#' @param ext: extension for the saved figures admitted by \code{\link[ggplot2]{ggsave}} (e.g., \code{".png"}, \code{".svg"}).
#' @return Plots of Rao's quadratic entropy, functional redundancy and Simpson's richness estimates over time (raw and normalised). If \code{save.plot} is \code{TRUE}, the plots will be saved too.
#' If the function is assigned it returns a list with two plots \code{plot1} and \code{plot2}, with the raw and normalised plots, respectively.
#' @details The normalisation is performed within each variable taking the minimum and maximum values.
#' @examples View(RaoSummary) # dataframe returned by DF_Rao
#'
#' @examples FD_plotRao(RaoSummary, save.plot=TRUE, ext=".png")
#' @seealso \code{\link{FD_Rao}}.
#'
#'
#'
FD_plotRao <- function(df, save.plot=TRUE, ext=".png"){
  pacman::p_load(ggplot2,viridis, rlist)
  plotsRao <- list()
  if(is.null(df$Time))stop("There is no time in the input dataframe")
  if(colnames(df)[1] != "Time")warning("The first column in your dataframe must be Time")
  if(is.null(df$Simpson))warning("There is no Simpson index in the input dataframe")
  if(is.null(df$FunRao))warning("There is no Rao index in the input dataframe")
  if(is.null(df$FunRedundancy))warning("There is no F-Redundancy in the input dataframe")

  norm_summary1 <- df
  for (j in 2:ncol(norm_summary1)) {
    norm_summary1[,j] <- as.numeric(as.character(norm_summary1[,j]))
  }
  norm_summary1 <- melt(norm_summary1, id=c("Time"))
  norm_summary1$norm_value <-NA
  for(i in 1:nrow(norm_summary1)){
    norm_summary1$norm_value[i] <- (norm_summary1$value[i]-min(norm_summary1[which(norm_summary1$variable == norm_summary1$variable[i]),]$value))/(max(norm_summary1[which(norm_summary1$variable == norm_summary1$variable[i]),]$value)-min(norm_summary1[which(norm_summary1$variable == norm_summary1$variable[i]),]$value))
  }
  a <- ggplot(data = norm_summary1, aes(x=Time, y = value, colour = variable, group= variable)) +
    geom_point() +
    geom_line()+
    labs(x ="Time", y = "Functional diversity measure", color="Measure") +
    scale_color_viridis_d()+
    theme_bw()+
    theme(axis.text.x = element_text(angle=90))

  b <- ggplot(data = norm_summary1, aes(x=Time, y = norm_value, colour = variable, group= variable)) +
    geom_point() +
    geom_line()+
    labs(x ="Time", y = "Functional diversity measure (normalized)", color="Measure") +
    scale_color_viridis_d()+
    theme_bw()+
    theme(axis.text.x = element_text(angle=90))

  print(a)
  print(b)
  #return(list.append(plotsRao, plot1=a, plot2=b))
  if(save.plot==TRUE){
    ggsave(plot = a, filename = paste0("RaoDiversitymeasures",ext), height = 12, width = 24, units = "cm")
    ggsave(plot = b, filename = paste0("RaoDiversitymeasuresNormalised",ext), height = 12, width = 24, units = "cm")
  }

}
