#' Summarise the null communities' functional diversity
#' @description This function summarises the dataframes returned by the function \code{\link{FD_nullCom}}. This function can be called directly from \code{\link{FD_nullCom}}.
#' @author Simon P. Castillo \email{spcastil@@uc.cl} & Felipe Opazo-Mella
#' @param nullrao: dataframe obtained after computing the function \code{\link{FD_nullCom}}.
#' @param nullFD: dataframe obtained after computing the function \code{\link{FD_nullCom}}.
#' @param nNull: numeric. The number of null replicates. This must be the same that those in \code{\link{FD_nullCom}}.
#' @param plot.null: \code{TRUE} or \code{FALSE}.If \code{TRUE this function passes the argument to the function \code{\link{FD_plotNull}}. Default \code{TRUE}.
#' @return This functions returns two dataframes \code{summarynullrao} and \code{summarynullFD} summarising the functional diversity simulated by the null models. Each dataframe includes the average values (m), the variance (v), and the lower and upper confidence intervals (\code{95%}).
#' Also, it returns and saves the corresponding plots if \code{plot.null} is \code{TRUE}.
#' @examples FD_nullSummary(nullrao = null_rao, nullFD = null_FDmetrics, nNull = 3, plot.null = FALSE)
#'
#' @examples View(summarynullrao)
#'
#' @examples View(summarynullFD)
#'
#'
#'
FD_nullSummary <- function(nullrao, nullFD, nNull, plot.null=TRUE){
  pacman::p_load(plyr)
  "%ni%" = Negate("%in%")
  if(is.data.frame(nullrao) != TRUE)stop("The nullrao object is not a data frame")
  if(is.data.frame(nullFD) != TRUE)stop("The nullFD object is not a data frame")


  for (i in 2:(ncol(nullrao))) {
    if(is.numeric(nullrao[,i]) == FALSE){
    nullrao[,i] <- as.numeric(levels(nullrao[,i]))[nullrao[,i]]
    }
  }

  times <-unique(as.character(nullrao$Time))
  summarynullrao = data.frame()

  for (n in 1:length(times)) {
    nulos = nNull
    summary1.2=nullrao[nullrao$Time == as.character(times[n]),] %>%
                  summarise(mSimpson = mean(Simpson, na.rm = T),
                      mFunRao = mean(FunRao, na.rm = T),
                      mFunRedun = mean(FunRedundancy, na.rm = T),
                      vSimpson = var(Simpson, na.rm = T),
                      vFunRao = var(FunRao, na.rm = T),
                      vFunRedun = var(FunRedundancy, na.rm = T),
                      Simpson.ic95= qt(1-((1-0.95)/2), (nulos- 1))* sqrt(var(Simpson)/ length(Simpson)),
                      Simpson.ic5= qt(1-((1-0.05)/2), (nulos - 1))* sqrt(var(Simpson)/ length(Simpson)),
                      FunRao.ic95= qt(1-((1-0.95)/2), (nulos- 1))* sqrt(var(FunRao)/ length(FunRao)),
                      FunRao.ic5= qt(1-((1-0.05)/2), (nulos - 1))* sqrt(var(FunRao)/ length(FunRao)),
                      FunRed.ic95= qt(1-((1-0.95)/2), (nulos- 1))* sqrt(var(FunRedundancy)/ length(FunRedundancy)),
                      FunRed.ic5= qt(1-((1-0.05)/2), (nulos - 1))* sqrt(var(FunRedundancy)/ length(FunRedundancy))
    )
    summarynullrao <- rbind(summarynullrao,summary1.2 )
  }

#  summarynullrao <<- summarynullrao
#}

  for (i in 2:(ncol(nullFD))) {
    if(is.numeric(nullFD[,i]) == FALSE){
      nullFD[,i] <- as.numeric(levels(nullFD[,i]))[nullFD[,i]]
    }
  }

  summarynullFD = data.frame()
  for (n in 1:length(times)) {
    nulos = nNull
    summary2.2=nullFD[nullFD$time == as.character(times[n]),] %>%
      summarise(mFEve = mean(FEve, na.rm = T),
                      mFDiv = mean(FDiv, na.rm = T),
                      mFSpe = mean(FSpe, na.rm = T),
                      mFRic = mean(FRic, na.rm = T),
                      mchull_PC12 = mean(chull_PC12, na.rm = T),
                      mchull_PC13 = mean(chull_PC13, na.rm = T),
                      vFEve = var(FEve, na.rm = T),
                      vFDiv = var(FDiv, na.rm = T),
                      vFSpe = var(FSpe, na.rm = T),
                      vFRic = var(FRic, na.rm = T),
                      vchull_PC12 = var(chull_PC12, na.rm = T),
                      vchull_PC13 = var(chull_PC13, na.rm = T),
                      FEve.ic95= qt(1-((1-0.95)/2), (nulos- 1))* sqrt(var(FEve)/ length(FEve)),
                      FEve.ic5= qt(1-((1-0.05)/2), (nulos - 1))* sqrt(var(FEve)/ length(FEve)),
                      FDiv.ic95= qt(1-((1-0.95)/2), (nulos- 1))* sqrt(var(FDiv)/ length(FDiv)),
                      FDiv.ic5= qt(1-((1-0.05)/2), (nulos - 1))* sqrt(var(FDiv)/ length(FDiv)),
                      FSpe.ic95= qt(1-((1-0.95)/2), (nulos- 1))* sqrt(var(FSpe)/ length(FSpe)),
                      FSpe.ic5= qt(1-((1-0.05)/2), (nulos - 1))* sqrt(var(FSpe)/ length(FSpe)),
                      FRic.ic95= qt(1-((1-0.95)/2), (nulos- 1))* sqrt(var(FRic)/ length(FRic)),
                      FRic.ic5= qt(1-((1-0.05)/2), (nulos - 1))* sqrt(var(FRic)/ length(FRic)),
                      chull_PC12.ic95= qt(1-((1-0.95)/2), (nulos - 1))*sqrt(var(chull_PC12)/ length(chull_PC12)),
                      chull_PC12.ic5= qt(1-((1-0.05)/2), (nulos - 1))*sqrt(var(chull_PC12)/ length(chull_PC12)),
                      chull_PC13.ic95= qt(1-((1-0.95)/2), (nulos - 1))*sqrt(var(chull_PC13)/ length(chull_PC13)),
                      chull_PC13.ic5= qt(1-((1-0.05)/2), (nulos - 1))*sqrt(var(chull_PC13)/ length(chull_PC13))
    )

    summarynullFD<- rbind(summarynullFD,summary2.2 )
  }

  summarynullrao$Time <- unique(as.character(nullrao$Time))
  summarynullFD$Time <- unique(as.character(nullFD$time))
  summarynullRao <<- summarynullrao
  summarynullFD <<- summarynullFD

  if(plot.null==TRUE){
    FD_nullPlot(d.rao = summarynullRao, d.FD = summarynullFD, plotRao = TRUE, saveRao = TRUE, plotFRic = TRUE, saveFRic = TRUE)
    }
} #ElFin
