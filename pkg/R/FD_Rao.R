#' Functional metrics (Simpson, Rao's quadratic Entropy and Funcional redundancy)
#' @description This functions calculates the Rao's quadratic entropy, Simpson's index and functional redundancy for each time bin along the temporal series.
#' @author Simon P. Castillo \email{spcastil@@uc.cl}, Felipe Opazo-Mella, Fabio Labra and Pablo Marquet
#' @param df: is a functional diversity dataframe obtained by using the function \code{\link{FD_df}}.
#' @return This function returns a dataframe with the Rao's complexity, Simpson's diversity, and functional redundancy estimates.
#' @note This function uses \code{\link[SYNCSA]{rao.diversity}} package for each time bin as a community.
#' @seealso  \code{\link[SYNCSA]{rao.diversity}}, \code{\link[FD]{gowdis}}
#' @examples RaoSummary <- FD_rao(df=data2)
#'
#' View(RaoSummary)
#' @references Rao, C.R. (1982). Diversity and dissimilarity coefficients: a unified approach. Theoretical Population Biology, 21, 24:43.
#'
#'
#'
FD_Rao = function(df){
  pacman::p_load(SYNCSA)

  times<-as.character(unique(df$time))
  summary1<-data.frame()

  for (i in 1:length(unique(df$time))) {
    stime<-times[i]
    #print(paste("time :", stime))

    dataAb<-t(df$abundance[which(df$time == stime)])
    colnames(dataAb)<- df$ecocode[which(df$time == stime)]
    rownames(dataAb) = stime

    ntraits = nchar(as.character(df$ecocode[1]))
    dataTraits<-df[which(df$time == stime),(ntraits+3):ncol(df)] # traits+abund+ecocode+time
    rownames(dataTraits) <- as.character(df[which(df$time == stime),]$ecocode)
    rd<-rao.diversity(dataAb, traits = dataTraits)
   summary1<-rbind(summary1, cbind(Time=stime, Simpson=rd$Simpson, FunRao=rd$FunRao, FunRedundancy=rd$FunRedundancy))
  }
  RaoSummary <-summary1
return(RaoSummary)
} #ElFin

