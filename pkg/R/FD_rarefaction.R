#' Compute rarefaction analyses
#' @description
#' @author Simon P. Castillo \email{spcastil@@uc.cl}, Felipe Opazo-Mella, Fabio Labra and Pablo Marquet
#' @param df: a dataframe containing the database of ocurrences at a given taxonomic level. It must match column names of \code{FD_data0}.
#' @param time: numeric. The number of iterations to run. Default 500.
#' @return The function saves three dataframes in your working directory name "Diversity.csv", "EcoGenera.csv" and "EcoOccurrences.csv".
#' @return "Diversity.csv": stores diversity and ecodiversity estimates by time bin.
#' @return "EcoOccurrences.csv": stores the number of ocurrences by ecocode over time.
#' @return "EcoGenera.csv": stores the number of genera by ecocode over time.
#' @examples FD_rarefaction(data=FD_data0, times=500)
#'
#'
#'

FD_rarefaction <- function(df,times= 500){

ecoall<- df
gencodes<- as.factor(round(tapply(ecoall$ECOCODE,ecoall$GENUS,mean)))
ecoall$ECOCODE<- as.factor(ecoall$ECOCODE)
ecoall$TIME = as.factor(ecoall$STG)
names(ecoall)
eco<- ecoall
tcov<- table(eco$TIME)				 # time coverage by occurrences
mylength<- function(x) y<- length(table(x))
ocov<- tapply(eco$COLL,eco$TIME,mylength)		# time coverage by number of collections
ss<-min(ocov)-1	#subsampling level (minimum coverage - 1)
tbins<- as.numeric(names(tcov))		# time bins

outdiv<- NULL
oute<- NULL
outeg<- NULL
for (i in tbins){
eco$TIME = as.character(eco$TIME)
 bin<- eco[eco$TIME==i,]		# select one time bin
 cid<- as.numeric(names(table(bin$COLL)))	# find all collections present in this bin
 div<- NULL
 ediv<- NULL
 egen<- NULL
 eocc2<- NULL
    for (n in 1:times){
      #print(paste0("replicate ", n, " out of ", times))
      one<- bin[which(bin$COLL %in% sample(cid,ss)),]
      gen<- table(droplevels(one$GENUS))
	    eg<- table(gencodes[which(names(gencodes) %in% names(gen))])
      ecot1<- table(droplevels(as.factor(one$ECOCODE)))
	    eocc1<- rbind(table(one$ECOCODE))
	    eocc2<- rbind(eocc2,eocc1)
      div<- rbind(div,length(gen))
      ediv<- rbind(ediv,length(ecot1))
      egen<- rbind(egen,eg)
      }
     divm<- c(i,median(div),mean(div),median(ediv),mean(ediv))
     eocc3<- apply(eocc2,2,mean)
     egen2<- apply(egen,2,mean)
     outdiv<- rbind(outdiv,divm)
     oute<- rbind(oute,eocc3)
     outeg<- rbind(outeg,egen2)
 }

egen_all<- cbind(tbins,outeg)		# number of genera per ecocategory by time bin
rownames(egen_all)<- 1:nrow(egen_all)
eocc_all<- cbind(tbins,oute)		# number of occurrences per ecocategory by time bin
rownames(eocc_all)<- 1:nrow(eocc_all)
colnames(outdiv)<- c("TIME","MED_DIV","MEAN_DIV","MED_EDIV","MEAN_EDIV")
rownames(outdiv)<- 1:nrow(outdiv)
outdiv<- as.data.frame(outdiv)	# diversity and ecodiversity estimates by time bin

# SAVE OUTPUTS TO EXTERNAL FILES ################
#write.csv(outdiv,"Diversity.csv")
write.csv(eocc_all,"EcoOccurrences.csv")
write.csv(egen_all,"EcoGenera.csv")
}
#ElFin
