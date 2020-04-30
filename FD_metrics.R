#' Estimate functional metrics
#' @description This function is based on Villeger et al. (view FD package). This function computes functional diversity metrics
#' @param
#'       df: a data.frame obtained after running the FD_df function.
#' @return
#'       FDsummary: data.frame. COntains the estimated parameters for functional diversity for each time bin. "FEve" functional evenness, "FDiv" functional divergence, "FSpe" functional specialisaiton, "FRic" functional richness.
#'       FuncSpace: list. Stores the Functional space for each time bin.
#'       ch1_list: list. Stores convex hull coordinates.
#'       ch2_list: list. Stores convex hull coordinates.
#'       pc1_list: list. Stores principal component coordinates.
#'       pc2_list: list. Stores principal component coordinates.
#'       pc3_list: list. Stores principal component coordinates.
#' @references
#'        Villéger, S., Mason, N. W., & Mouillot, D. (2008). New multidimensional functional diversity indices for a multifaceted framework in functional ecology. Ecology, 89(8), 2290-2301.


FD_metrics <- function(df){
  pacman::p_load(reshape2,stringr, foreach, doParallel, rowr)
  "%ni%" =  Negate("%in%")
  times<-as.character(unique(df$time))
  summary2<-data.frame()
  FuncSpace <- list()
  ch1_list <- list()
  ch2_list <- list()
  pc1_list <- list()
  pc2_list <- list()
  pc3_list <- list()
  for (qq in 1:length(times)) {

    print(paste("time: ",times[qq]))
    weight <- as.data.frame(t(subset(df, time== times[qq], select = "abundance")))
    coord  <- subset(df, time== times[qq], select = 2:(nchar(as.character(df$ecocode[1]))+1))
    nnn    <- subset(df, time== times[qq], select = "ecocode")
    colnames(weight)<-rownames(coord)<-nnn$ecocode
    rownames(weight)<- times[qq]
    names <- names(weight[which(weight == 0)])

    coord<-coord[which(rownames(coord) %ni% names),]
    weight <- weight[(colnames(weight) %ni% names)]
    for (n in 1:ncol(coord)) {
      coord[,n] = as.numeric(coord[,n])
    }

    GT_pca<- prcomp(coord, retx=TRUE, center=TRUE, scale =TRUE)
    pc1<-round(as.data.frame(summary(GT_pca)[6])[2,1],3)
    pc2<-round(as.data.frame(summary(GT_pca)[6])[2,2],3)
    pc3<-round(as.data.frame(summary(GT_pca)[6])[2,3],3)
    ch1<-paste0("pc1_", times[qq])
    ch2<-paste0("pc2_", times[qq])
    ch3<-paste0("pc3_", times[qq])
    assign(ch1, pc1)
    assign(ch2,pc2)
    assign(ch3, pc3)

    tGT_pca <- GT_pca$x
    GT_dat <- data.frame(tGT_pca)
    GPC1<-GT_dat$PC1
    GPC2<-GT_dat$PC2
    GPC3<-GT_dat$PC3
    df0 <-rbind(GPC1,GPC2)
    df1=rbind(GPC1,GPC3)
    df10<-t(df0)
    df11=t(df1)
    con.hull.pos1 <- chull(df10)
    con.hull.pos2 <- chull(df11)
    con.hull1 <- rbind(df10[con.hull.pos1,],df10[con.hull.pos1[1],])
    con.hull2 <- rbind(df11[con.hull.pos2,],df11[con.hull.pos2[1],])

    FDx= FDind(traits = coord, abundances = weight)
    FRic<- FDx$FRic
    FSpe<- FDx$FSpe
    FDiv<- FDx$FDiv
    FEve<- FDx$FEve

    chull_PC12 <- convhulln(cbind(GPC1, GPC2), "FA")$area#hullarea(GPC1,GPC3)
    chull_PC13 <- convhulln(cbind(GPC1, GPC3), "FA")$area#hullarea(GPC1,GPC3)
    PCS<-data.frame(GT_dat)
    ab<-t(weight)
    ab<-data.frame(ab)
    PCS<-cbind.fill(PCS,ab)
    PCS<-data.frame(PCS)
    rownames(PCS)<-rownames(ab)
    con.hull1<-data.frame(con.hull1)
    con.hull2<-data.frame(con.hull2)
    datamultidim1<-c(time=times[qq],FEve=FEve, FDiv=FDiv, FSpe=FSpe,FRic = FRic, chull_PC12= chull_PC12, chull_PC13=chull_PC13)
    summary2.1<-as.data.frame(t(datamultidim1))
    colnames(summary2.1)<-c( "time","FEve","FDiv","FSpe","FRic","chull_PC12", "chull_PC13")
    summary2 <- rbind(summary2, summary2.1)
    npcs<-paste0("PCS_", times[qq])
    assign(npcs, PCS)
    ch1<-paste0("con.hull1_", times[qq])
    ch2<-paste0("con.hull2_", times[qq])
    assign(ch1, con.hull1)
    assign(ch2, con.hull2)
    FuncSpace[[qq]] <-PCS
    ch1_list[[qq]] <-con.hull1
    ch2_list[[qq]] <-con.hull2
    pc1_list[[qq]] <- pc1
    pc2_list[[qq]]<- pc2
    pc3_list[[qq]] <- pc3
    FDsummary <- summary2
  }
  FDsummary<<-summary2
  FuncSpace <<- FuncSpace
  ch1_list<<- ch1_list
  ch2_list<<- ch2_list
  pc1_list<<- pc1_list
  pc2_list<<- pc2_list
  pc3_list<<- pc3_list

  return(summary2)

}

