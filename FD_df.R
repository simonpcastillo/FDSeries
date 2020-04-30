#' Transforming a timexlife-mode dataframe into a functional diversity dataframe
#' @description  This function transforms an input data frame 'df' into a functional diversity dataframe.
#' @param
#'   df: input data.frame with rows indicating time bins and columns indicating each life mode or ecocode. The df values are estimates of abundance.//
#'   subcat: integer. The number of subcategories for each trait.
#' @return This functions returns a data.frame called data1; it is a functional diversity dataframe with an extension of the traits defined by the ecocode
#'         and the corresponding estimation of abundance or biomass for each time slice.


FD_df <- function(df, subcat=6){
pacman::p_load(reshape2,stringr, foreach, doParallel)
timeSums <- colSums(df)
for (i in 1:nrow(df)) {
  for(j in 1:ncol(df)){
    df[i,j] <- df[i,j]/timeSums[j]
  }
}
data1<-melt(df)
data1<-cbind(rep(rownames(df, ncol(df))),data1)
ntraits = nchar(as.character(data1$variable[1]))
names(data1) = c("time", "ecocode", "abundance")
data1<- cbind(data1$ecocode,
              as.data.frame(str_split_fixed(data1$ecocode, n=ntraits, pattern = "")),
              data1$time,
              data1$abundance)
names(data1) <- c("ecocode",letters[1:ntraits],"time", "abundance")
traits<-as.data.frame(matrix(0, ncol = ntraits*subcat, nrow = nrow(data1)))
colnames(traits)<-paste0(rep(letters[1:ntraits], each=subcat),1:subcat)
data1<-cbind(No=array(NA, nrow(data1)),data1,traits)

for (j in 1:nrow(data1)) {
#foreach (j=1:nrow(data1)) %dopar% {
  for(k in 3:(ntraits+2)){
    b <- as.numeric(data1[j,k])
    data1[j,(subcat*(k-1)+b)]<-1
  }
}
data1<<- data1[,-1]
}
