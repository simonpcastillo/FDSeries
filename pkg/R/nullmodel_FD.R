#' Internal use
#' @description This function is for internal use; it computed the null models. There are three options based on the resampling routine.
#' @param com: a timexlife-mode dataframe. time bins in rows and life-mode in columns.
#' @param type: Type of resampling. \code{"col-wise"} relocates abundances within a life-mode, \code{"row-wise"} relocates abundances wihtin a time bin, and \code{"all"} relocate abundances across time bins and life-modes.
#' @return This function returns a resampled data.frame (null community).
#' @author Simon P. Castillo \email{spcastil@@uc.cl} & Felipe Opazo-Mella
#' @examples
#' nullcommunity <- nullmodel_FD(com=data0, type="all")
#'
#'
#'
nullmodel_FD <- function(com,type="all"){
  if(type == "col-wise"){  # Relocate abundances within a column
    for (i in 1:ncol(com)) {
      com[,i] <- sample(com[,i], size = length(com[,i]), replace = F)
    }
  }
  if(type == "row-wise"){ # Relocate abundances within a row
    for (i in 1:nrow(com)) {
      com[i,] <- sample(com[i,], size = length(com[i,]), replace = F)
    }
  }
  if(type == "all"){ # Relocate abundances randomly within the matrix
    com1 = com
    for (i in 1:ncol(com)) {
      for(j in 1:nrow(com)){

        com1[j,i] <- sample(as.matrix(com), size=1)

      }
    }

    com = com1
  }
  return(com)

}#ElFin


