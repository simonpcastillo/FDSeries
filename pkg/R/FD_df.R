#' Transforming a time-bins x life-mode dataframe into a functional diversity dataframe
#' @description  This function transforms an input dataframe \code{df} into a functional diversity dataframe.
#' @author Simon P. Castillo \email{spcastil@@uc.cl}, Felipe Opazo-Mella, Fabio Labra and Pablo Marquet
#' @param df: \code{dataframe}. Input dataframe with row names indicating time bins and column names indicating each life-mode or ecocode. The \code{df} values are estimates of the abundance of each life-mode at each time bin.
#' @param subcat: \code{numeric vector}. The number of subcategories for each trait.Its length must be equal that the number of traits of each life-mode.
#' @return This function returns a functional diversity dataframe with an extension of the traits defined by the ecocode
#'         and the corresponding occurrence estimation for each time bin.
#' @details  An \emph{ecocode} is defined by a numeric sequence of traits as integers (e.g., 11211234); hence, the length of the \code{subcat} argument must be eight and its values cannot be numerically lower than the respective values in the ecocode's traits.
#' @examples View(data1) #internal dataset
#'
#' @examples data2 <- Fd_df(df=data1)
#'
#' @examples View(data2) #functional diversity dataframe
#'
#'
#'
FD_df <- function(df, subcat){
  pacman::p_load(reshape2,stringr)
  if(length(subcat) != nchar(colnames(df)[1]))stop("Length of subcat must be equal that the number of traits for the life-modes.")
  if(class(df) != 'data.frame') stop('df must be a dataframe')
  timeSums <- colSums(df)
  for (i in 1:nrow(df)) {
    for(j in 1:ncol(df)){
      df[i,j] <- df[i,j]/timeSums[j]
    }
  }
  data1<-melt(df, id.vars = NULL)
  data1<-cbind(rep(rownames(df, ncol(df))),data1)
  names(data1) = c("time", "ecocode", "abundance")
  ntraits = nchar(as.character(data1$ecocode[1]))

  data1<- cbind(data1$ecocode,
                as.data.frame(str_split_fixed(data1$ecocode, n=ntraits, pattern = '')),
                data1$time,
                data1$abundance)
  names(data1) <- c("ecocode",letters[1:ntraits],"time", "abundance")
  traits<-as.data.frame(matrix(0, ncol = sum(subcat), nrow = nrow(data1)))
  colnames(traits)<-paste0(rep(letters[1:ntraits], subcat),sequence(subcat))
  data1<-cbind(No=array(NA, nrow(data1)),data1,traits)
  data1 <- data1[,-1]

  for (j in 1:nrow(data1)) {
    for(k in 2:(ntraits+1)){
      b <- as.numeric(data1[j,k])
      st <- sum(subcat[1:(k-1)]) - subcat[(k-1)]
      data1[j,(b + st +ntraits + 3)]<-1
    }
  }
  #data1<<- data1
  return(data1)
}#ElFin
