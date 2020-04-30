#' Internal use
#' @description
#'       For internal use. Computes functional diversity metrics for a single community (one time bin). See refernece below.
#' @param data.frame
#' @return data.frame
#' @references
#'        Villéger, S., Mason, N. W., & Mouillot, D. (2008). New multidimensional functional diversity indices for a multifaceted framework in functional ecology. Ecology, 89(8), 2290-2301.

##########################################################################################################################
#  function to compute the 4 functional diversity indices                                                                #
#  this is an EDITED version of de original used in Villeger et al. 2009, Ecological Applications (in press)                                                      #
#                                                                                                                        #
# - Functional richness (FRic), Functional evenness (FEve), Functional divergence (FDiv)                                 #
#    Villeger et al. 2008 (Ecology, 89: 2290-2301)                                                                       #
#                                                                                                                        #
# - Functional specialization (FSpe) following Bellwood et al. 2006 (Proc. R. Soc. B., 273: 101-107)                     #
#                                                                                                                        #
#                                                                                                                        #
# This function requires R libraries 'ape' and 'geometry'                                                                #
#                                                                                                                        #
#                                                                                                                        #
# inputs:                                                                                                                #
#        - "traits" = functional matrix (S x T) with values of the T functional traits for the S species of interest     #
#           at least 2 traits, with numeric values (NA are not allowed)                                                  #
#           for each trait, values are standardized (mean=0 and standard deviation=1)                                    #
#                                                                                                                        #
#       - "abundances" = abundance matrix (C x S) with the abundances of the S species in the C commnuities studied      #
#           NA are automatically replaced by 0                                                                           #
#           for FRic computation, number of species must be higher than number of traits                                 #
#                                                                                                                        #
# output:                                                                                                                #
#         a dataframe with indices values in the C communities (row names are those given in 'abundances')               #
#               - Nbsp: number of species                                                                                #
#               - FRic: functional richness index                                                                        #
#               - FEve: functional evenness index                                                                        #
#               - FDiv: functional divergence index                                                                      #
#               - FSpe: functional specialization index                                                                  #
##########################################################################################################################

FDind<-function(traits,abundances)  {

#loading required libraries
pacman::p_load(geometry, ape)

traits = data.frame(traits)
for (o in 1:ncol(traits)) {
  traits[,o] = as.numeric(traits[,o])
}
traits=as.matrix(traits)


abundances = data.frame(abundances)
for (o in 1:ncol(abundances)) {
  abundances[,o] = as.numeric(abundances[,o])
}
abundances=as.matrix(abundances)

# T = number of traitscoor
T<-dim(traits)[2]

# c = number of communities
C<-dim(abundances)[1]

# check coherence of number of species in 'traits' and 'abundances'
if (dim(abundances)[2]!=dim(traits)[1]) stop(" error : different number of species in 'traits' and 'abundances' matrices ")

# check format of traits values
if (ncol(traits)<2) stop ("'traits' must have at least 2 columns")
if (is.numeric(traits)==F) stop ("traits values must be numeric")

# check absence of NA in 'traits'
if (length(which(is.na(traits)==T))!=0) stop(" error : NA in 'traits' matrix ")

# replacement of NA in 'abundances' by '0'
abundances[which(is.na(abundances))]<- 0

# definition of vector for results, with communities'names as given in 'abundances'
Nbsp<-rep(NA,C) ; names(Nbsp)<-row.names(abundances)
FRic<-rep(NA,C) ; names(FRic)<-row.names(abundances)
FEve<-rep(NA,C) ; names(FEve)<-row.names(abundances)
FDiv<-rep(NA,C) ; names(FDiv)<-row.names(abundances)
FSpe<-rep(NA,C) ; names(FSpe)<-row.names(abundances)

# scaling and centering of each trait according to all species values
traitsCS<-scale(traits, center=TRUE, scale=TRUE)

# functional specialization of each species (distance to point 0,0 in the standardized functional space)
FSpeS<-( apply(traitsCS, 1, function(x) {x%*%x} ) )^0.5

############################################################################################################

for (i in 1:C){
  # selection of species present in the community
  esppres<-which(abundances[i,]>0)

  #  number of species in the community
  S<-length(esppres) ; Nbsp[i]<-S

  # check if more species tan traits
 if ( S<=T) stop(paste(" number of species must be higher than number of traits in community:",row.names(abundances)[i] ))

  # filter on 'traits' and 'abundances' to keep only values of species present in the community
  tr<-traitsCS[esppres,] ;  ab<-as.matrix(abundances[i,esppres] )
  tr[is.na(tr)] <- 0    # scaling of abundances

  abondrel<-ab/sum(ab)

  # functional diversity indices

      # FRIC

        # use of convhulln function

              # volume
              FRic[i]<-round(convhulln(tr,"FA")$area,6)

              # identity of vertices
              vert0<-convhulln(tr,"Fx TO 'vert.txt'")
              vert1<-scan("vert.txt",quiet=T)
			        vert2<-vert1+1
              vertices<-vert2[-1]


      # FEve

        # computation of inter-species euclidian distances
              distT<-dist(tr, method="euclidian")

        # computation of Minimum Spanning Tree and conversion of the 'mst' matrix into 'dist' class
              linkmst<-mst(distT) ; mstvect<-as.dist(linkmst)

        # computation of the pairwise cumulative relative abundances and conversion into 'dist' class
              abond2<-matrix(0,nrow=S,ncol=S)
              for (q in 1:S)
              for (r in 1:S)
              abond2[q,r]<-abondrel[q]+abondrel[r]
              abond2vect<-as.dist(abond2)  # end of q,r

        # computation of EW for the (S-1) segments pour relier les S points
              EW<-rep(0,S-1)

              flag<-1
              for (m in 1:((S-1)*S/2))
              {if (mstvect[m]!=0) {EW[flag]<-distT[m]/(abond2vect[m]) ; flag<-flag+1}}  # end of m

        # computation of the PEW and comparison with 1/S-1, finally computation of FEve
              minPEW<-rep(0,S-1)  ;  OdSmO<-1/(S-1)
              for (l in 1:(S-1))
                minPEW[l]<-min( (EW[l]/sum(EW)) , OdSmO )  # end of l
              FEve[i]<-round( ( (sum(minPEW))- OdSmO) / (1-OdSmO ) ,6)


      # FDiv

        # traits values of vertices
              trvertices<-tr[vertices,]

        # coordinates of the center of gravity of the vertices (Gv)
              baryv<-apply(trvertices,2,mean)

        # euclidian dstances to Gv (dB)
              distbaryv<-rep(0,S)
              for (j in 1:S)
                distbaryv[j]<-( sum((tr[j,]-baryv)^2) )^0.5  # end of j

        # mean of dB values
              meandB<-mean(distbaryv)

        # deviations to mean of db
              devdB<-distbaryv-meandB

        # relative abundances-weighted mean deviation
              abdev<-abondrel*devdB

        # relative abundances-weighted mean of absolute deviations
              ababsdev<-abondrel*abs(devdB)

        # computation of FDiv
              FDiv[i]<-round( (sum(abdev)+meandB) / (sum(ababsdev)+meandB) ,6)

      # FSpe
        # mean functional specialization in the communities
              FSpe[i]<-(abundances[i,]/sum(abundances[i,]))%*%FSpeS

} # end of i



# result storage

res<-data.frame(Nbsp=Nbsp, FRic=FRic,FEve=FEve, FDiv=FDiv, FSpe=FSpe )
return(res)

}#end
