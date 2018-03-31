#Function to assess PSM matches and requires specification of the standard deviation used in the diagnostic function.
################Balance and/or Baseline Equivalency###############
Baseline_equiv_fn<-function(MatchedSets,Treatvar,ID_vars,pre_pooled_sd,Match_num="Match_num",SD_mean_before=NULL,ref_var=NULL,other_psm=NULL,plot.points=T,outlier_dist=3,global=T){
  
  '%notin%'<-Negate('%in%')
  options(useFancyQuotes = F)
  if(global){
    suppressPackageStartupMessages(require(openxlsx))
    suppressPackageStartupMessages(require(data.table))
    suppressPackageStartupMessages(require(lattice))
    suppressPackageStartupMessages(require(latticeExtra))
    suppressPackageStartupMessages(require(gridExtra))
    suppressPackageStartupMessages(require(maptools))
    suppressPackageStartupMessages(require(DescTools))
  } else {
    require(openxlsx)
    require(data.table)
    require(lattice)
    require(latticeExtra)
    require(gridExtra)
    require(maptools)
    require(DescTools)
  }
  
  
  ll<-lapply(as.list(match.call(expand.dots = F))[c("Treatvar","ID_vars")],as.name)
  dataname<-pairlist_converter(as.list(match.call())$MatchedSets)
  
  ######Shaping pre-pooled SDs in format necessary for this function###################
  if(is.null(names(pre_pooled_sd))|(class(pre_pooled_sd)[match("data.frame",class(pre_pooled_sd))]!="data.frame"&class(pre_pooled_sd)[match("list",class(pre_pooled_sd))]!="list"&class(pre_pooled_sd)[match("numeric",class(pre_pooled_sd))]!="numeric")){stop(paste("You need to enter a valid Pooled SD measure. This can be either a list, dataframe, or named number object with pooled standard deviations based on the full sample corresponding to the names of the covariates used in the matching process. Length of pooled SD column or vector must be equal to the number of covariates in your",sQuote(ll$MatchedSets),"not including",paste(ll$Treatvar,ll$ID_vars,"or",Match_num,sep=",")))}
  
  
  if(is.data.frame(pre_pooled_sd)){
    if(length(pre_pooled_sd)==2){
      num_col<-which(sapply(pre_pooled_sd,is.numeric))
      char_col<-which(sapply(pre_pooled_sd,function(i) class(i)!="numeric"))
      pooled_format<-setnames(data.table(pre_pooled_sd),c(char_col,num_col),c("Covariates","Pooled_SD"))[,Covariates:=as.character(Covariates)]} else{pooled_format<-data.table(setNames(data.frame(t(pre_pooled_sd)),"Pooled_SD"),keep.rownames = "Covariates")[,Covariates:=as.character(Covariates)]}
  } else{
    pooled_format<-listrep2datatable_fn(as.list(pre_pooled_sd),left_varname = "Covariates",right_varname = "Pooled_SD")[,Covariates:=as.character(Covariates)]
  }
  
  ##Control flow for whether to present before and after SMD and also as a check for same variables in Matched Sets and pre-pooled_SDs#######
  if(!is.null(ref_var)&&!is.null(SD_mean_before)){
    X<-ref_var[data.table(MatchedSets),on=c(ID_vars,Treatvar)][,c(pooled_format$Covariates,Treatvar,other_psm,Match_num,ID_vars),with=F]
    Compare_ind<-T
  } else if(is.null(ref_var)&&!is.null(SD_mean_before)){
    X<-data.table(MatchedSets)[,c(pooled_format$Covariates,Treatvar,other_psm,Match_num,ID_vars),with=F]
    Compare_ind<-T
  } else {
    Compare_ind<-F
    if(length(which(pooled_format$Covariates%notin%colnames(MatchedSets)))!=0){
      stop(paste(paste(pooled_format$Covariates[which(pooled_format$Covariates%notin%colnames(MatchedSets))],collapse = ","),"are not in",sQuote(dataname)))
    } else {
        X<-data.table(MatchedSets)[,c(pooled_format$Covariates,Treatvar,other_psm,Match_num,ID_vars),with=F]
        print("No before and after comparison is possible based on your specified options. If you wish to produce a before and after matching comparison specify a dataframe in the function options for the standardized means before matching.")
        }
  }
  
  ####Verifying the correct treatment group
  if(is.numeric(MatchedSets[[Treatvar]])){print(paste(sQuote(match.call()$Treatvar),"is not a factor.  Assuming",sQuote(max(X[[Treatvar]])),"is the treatment group."));X[[Treatvar]]<-factor(plyr::mapvalues(X[[Treatvar]],sort(unique(X[[Treatvar]])),c("Comparison","Treatment")))} else{X[,eval(Treatvar):=lapply(.SD,factor),.SDcols=Treatvar]}
  

  Match_weights_means<-dcast(melt(copy(X)[,-c(ID_vars),with=F],id.vars=c(Treatvar,Match_num),variable.factor=F),as.formula(paste(paste0(c(Treatvar,Match_num),collapse="+"),"~","variable")),fun=list(N=length,Weighted=mean))
  
  
  Match_weights<-setnames(copy(Match_weights_means)[,c(1:3),with=F],3,"Match_num_wts")
  
  
  colnames(Match_weights_means)[grep("value_mean_",colnames(Match_weights_means))]<-gsub("value_mean_","",grep("value_mean_",colnames(Match_weights_means),value=T))
  
  Match_means<-copy(Match_weights_means)[,c(Match_num,Treatvar,grep("Weighted",colnames(Match_weights_means),value = T)),with=F]
  
  Weighted_covariates<-copy(Match_weights)[copy(Match_means),on=c(Match_num,Treatvar)][copy(X),on=c(Match_num,Treatvar)]
  
  Weighted_covariates_graph<-copy(Match_weights)[copy(Match_means),on=c(Match_num,Treatvar)]
  colnames(Weighted_covariates_graph)[grep("Weighted.",colnames(Weighted_covariates_graph))]<-gsub("Weighted.","",grep("Weighted.",colnames(Weighted_covariates_graph),value=T))
  
  
  
  Group_means_sds<-Reduce(function(...) merge(...),setattr(lapply(colnames(Weighted_covariates_graph)[-match(c(Treatvar,Match_num,"Match_num_wts"),colnames(Weighted_covariates_graph))],function(j) setkeyv(Weighted_covariates_graph[is.na(Weighted_covariates_graph[[j]])==F,lapply(.SD,function(i) list(Mean=mean(i,na.rm=T),SD=sd(i,na.rm=T))),by=Treatvar,.SDcols=j][,Stat:=rep(c("Mean","SD"),length(unique((Weighted_covariates_graph[[Treatvar]]))))],c(Treatvar,"Stat"))),"names",colnames(Weighted_covariates_graph)[-match(c(Treatvar,Match_num,"Match_num_wts"),colnames(Weighted_covariates_graph))]))[,lapply(copy(.SD),unlist)]
  
  
  
  Match_mean_diff<-listrep2datatable_fn(as.list(copy(Weighted_covariates_graph)[,-c(Match_num,"Match_num_wts"),with=F][,lapply(copy(.SD),mean),by=Treatvar][,sapply(copy(.SD),function(i) abs(diff(unlist(i)))),.SDcols=-c(Treatvar)]),left_varname = "Covariates",right_varname = "Mean_diffs")
  
  final_results<-dcast(melt(copy(Group_means_sds),id.vars=c(Treatvar,"Stat"),variable.name="Covariates",variable.factor=F),paste("Covariates","~",paste0(c("Stat",Treatvar),collapse = "+")))[Match_mean_diff,on="Covariates"][pooled_format,on="Covariates"][,SD_mean_diff:=Mean_diffs/Pooled_SD]
  
  PSM_var<-grep("psm",pooled_format$Covariates,value = T)
  if(length(PSM_var)==0){
    if(is.null(other_psm)){
      print("Unable to detect psm variable. Please specify by including the variable name as a character vector in 'other_psm' parameter of function for psm analysis if you wish to get more analysis for it.")
    } else {PSM_var=other_psm}
  }
  ll_up<-append(ll,list(PSM_var=as.name(PSM_var),Match_num=as.name(Match_num)))
  
  if(!is.null(PSM_var)){
    PSM_summary<-PSM_graph_data<-copy(Weighted_covariates_graph)[is.na(eval(ll_up$PSM_var))==F][,PSM_Dist:=abs(diff(eval(ll_up$PSM_var))),by=Match_num][,list(N=.N,Mean=mean(PSM_Dist,na.rm=T),SD=sd(PSM_Dist,na.rm=T),Median=median(PSM_Dist,na.rm=T),Range=(paste0(format(round(range(PSM_Dist,na.rm=T),4),nsmall = 4),collapse = "-")))]
    
    ###########Graphing############
    plotcolors<-c("cyan","magenta")
    PSM_graph_data<-copy(Weighted_covariates_graph)[is.na(eval(ll_up$PSM_var))==F][,Dist:=diff(eval(ll_up$PSM_var)),by=Match_num][,Direction:=ifelse(Dist>=0,"Positive","Negative")]
    
    
    
    PSM_graph_data_match<-dcast(copy(PSM_graph_data),paste(paste0(c(Match_num,"Dist","Direction"),collapse = "+"),"~",Treatvar),value.var=PSM_var)[,Outlier:=ifelse(abs(Dist)>=outlier_dist*sd(Dist),"Outlier","Under3SD")]
    
    OL_dist=abs(PSM_graph_data_match$Dist[PSM_graph_data_match$Outlier=="Outlier"])
    Dist_SD<-sd(abs(PSM_graph_data_match$Dist))
    
    if(length(OL_dist)>0){
      DistGraphScaleTicks=seq(0,eval(max(OL_dist)+Dist_SD),by=Dist_SD/4)
    } else { DistGraphScaleTicks<-NULL}
    
    
    
    PSM_hist_after<-histogram(as.formula(paste("~",PSM_var,"|",Treatvar)),data=Weighted_covariates_graph,group=eval(ll$Treatvar),type="count",main=paste("Predicted PSM Scores by",sQuote(ll$Treatvar)),xlab=c("PSM Scores"),col=plotcolors,strip=strip.custom(bg="lightgrey",par.strip.text=list(cex=0.7,fontface="bold")),scales=list(alternating=F,tck=c(1,0)),panel=function(x,col=col,...){panel.histogram(x,col=col[packet.number()],...)})
    
    PSM_dens_after<-densityplot(as.formula(paste("~",PSM_var)),data=PSM_graph_data,group = eval(ll$Treatvar),auto.key = list(border=T,background="lightgrey",fontface="bold",cex=0.7,corner=c(1,1)),main=paste("Density Plot of Predicted PSM Scores by",sQuote(ll$Treatvar)),xlab=c("PSM Scores"),scales = list(tck=c(1,0)),plot.points=plot.points)
    
    grid.arrange(PSM_hist_after,PSM_dens_after,ncol=1)
    
    RightMarginKey<-list(title=expression(paste(underline(bold("Distance Direction")))),space="right",columns=1,border=F,fontface="bold",cex=0.7,cex.title=0.7)
    UpperMarginKey<-list(title=expression(paste(bold("Distance Direction"))),space="top",columns=2,border=T,background="lightgrey",fontface="bold",cex=0.7,cex.title=0.8)
    
    PSM_aft_hist<-histogram(as.formula(paste("~",PSM_var,"|",Treatvar)),data=eval(PSM_graph_data[is.na(eval(ll_up$PSM_var))==F]),group=eval(ll$Treatvar),type="count",main=paste("Predicted PSM Scores by",sQuote(ll$Treatvar)),xlab=c("PSM Scores"),col=plotcolors,strip=strip.custom(bg="lightgrey",par.strip.text=list(cex=0.7,fontface="bold")),scales=list(x="same",y="free",alternating=F,tck=c(1,0)),panel=function(x,col=col,...){panel.histogram(x,col=col[packet.number()],...)})
    
    Matched_dist_comparisons<-xyplot(as.formula(paste(sort(levels(PSM_graph_data[[Treatvar]]),decreasing = T),collapse = "~")),data=PSM_graph_data_match,group=Direction,pch=21,cex=1,col=plotcolors,main=paste("PSM Score by Matched Pair "),auto.key=RightMarginKey,scales=list(tck=c(1,0)),aspect="iso",panel=function(x,y,...){
      panel.xyplot(x,y,...) 
      panel.abline(a=0,b=1,col="black")}
    )
    
    if(is.null(DistGraphScaleTicks)==F){
      Matched_dist_comparisons<-update(Matched_dist_comparisons,main="PSM Score by Matched Pair with Labeled Outliers")+as.layer(xyplot(as.formula(paste(sort(levels(PSM_graph_data[[Treatvar]]),decreasing = T),collapse = "~")),data=copy(PSM_graph_data_match)[Outlier=="Outlier"],group=Direction,pch=21,cex=1,col=plotcolors,panel=function(x,y,data=copy(PSM_graph_data_match)[Outlier=="Outlier"],...){
        panel.xyplot(x,y,...)
        panel.pointLabel(x=x,y=y,labels=data$Match_num,cex=0.5)
        panel.abline(a=0,b=1,col="black")
      }),under=F)
      
      
      Outlier_only<-xyplot(as.formula(paste(sort(levels(PSM_graph_data[[Treatvar]]),decreasing = T),collapse = "~")),data=copy(PSM_graph_data_match)[Outlier=="Outlier"],group=Direction,pch=21,cex=1,col=plotcolors,auto.key=RightMarginKey,main=paste0("Matched Pairs of PSM Scores for Outliers Only (",outlier_dist," SDs)"),scales=list(tck=c(1,0)),at=NULL,panel=function(x,y,data=copy(PSM_graph_data_match)[Outlier=="Outlier"],...){
        panel.xyplot(x,y,...)
        panel.text(x=1,y=1,labels=paste("y=x"))
        panel.pointLabel(x=x,y=y,labels=data$Match_num,cex=0.8,fontface = "bold")
        panel.abline(a=0,b=1,col="red")})
      
      Outlier_dist<-xyplot(as.formula(paste(c(substitute(abs(Dist)),Match_num),collapse="~")),data=copy(PSM_graph_data_match)[Outlier=="Outlier"],group=Direction,type=c("p"),pch=21,cex=1,col=plotcolors,auto.key=RightMarginKey,main=paste0("Absolute PSM Score Distance for Outliers (",outlier_dist," SDs)"," of Matched Units"),xlab="Match Numbers",ylab="PSM Score Distance",ylim=c(0,eval(max(OL_dist)+Dist_SD)),scales=list(x=list(draw=F),y=list(relation="same",tck=c(1,0))),panel=function(x,y,data=copy(PSM_graph_data_match)[Outlier=="Outlier"],...){
        panel.xyplot(x,y,...)
        panel.pointLabel(x=x,y=y,labels=data$Match_num,cex=0.8,fontface = "bold")
      })
      grid.arrange(Matched_dist_comparisons,Outlier_dist,ncol=1,heights=c(2,1))
    } else{Outlier_only="None"}
    
  } else{Matched_dist_comparisons=NULL}##This ends the psm analysis.  
  
  if(global){
    dataname<-as.character(dataname)
    assign(paste(dataname,"with_weights",sep="_"),Weighted_covariates,envir = .GlobalEnv)
    assign(paste("Desc_stats",dataname,sep="_"),Group_means_sds,envir = .GlobalEnv)
    assign(paste("SDmeans",dataname,sep="_"),copy(final_results)[,c("Covariates","SD_mean_diff"),with=F],envir = .GlobalEnv)
    assign(paste("Compare",dataname,sep="_"),setnames(SD_mean_before[copy(final_results)[,c("Covariates","SD_mean_diff"),with=F],on="Covariates"][,c("SD_mean_diff","i.SD_mean_diff"):=lapply(.SD,function(i) format(round(i,4),nsmall=4)),.SDcols=c("SD_mean_diff","i.SD_mean_diff")],c("Covariates","SD_means_before_matching","SD_means_after_matching")),envir = .GlobalEnv)
  }
  
  if(Compare_ind){
    cat("\n\tSummary of Propensity Score Differences Between Matches\n-----------------------------------------------------------------------------\n")
    print(PSM_summary)
    return(list(Full_data_wts=Weighted_covariates,Matched_data_wts=Weighted_covariates_graph,Descriptive_stats_wts=Group_means_sds,All_calc=final_results[,colnames(final_results)[-match("Covariates",colnames(final_results))]:=lapply(.SD,function(i) round(i,4)),.SDcols=colnames(final_results)[-match("Covariates",colnames(final_results))]],Compare=setnames(SD_mean_before[copy(final_results)[,c("Covariates","SD_mean_diff"),with=F],on="Covariates"],c("Covariates","SD_means_before_matching","SD_means_after_matching")),PSM_summary=PSM_summary,All_PSMmatches=Matched_dist_comparisons,Outliers_only=Outlier_only,PSM_Hist=PSM_aft_hist))
    
  } else {
    cat("\n\tSummary of Propensity Score Differences Between Matches\n-----------------------------------------------------------------------------\n")
    print(PSM_summary)
    return(list(Full_data_wts=Weighted_covariates,Matched_data_wts=Weighted_covariates_graph,Descriptive_stats_wts=Group_means_sds,All_calc=final_results[,colnames(final_results)[-match("Covariates",colnames(final_results))]:=lapply(.SD,function(i) round(i,4)),.SDcols=colnames(final_results)[-match("Covariates",colnames(final_results))]],SD_means=final_results[,c("Covariates","SD_mean_diff"),with=F],PSM_summary=PSM_summary,All_PSMmatches=Matched_dist_comparisons,Outliers_only=Outlier_only,PSM_Hist=PSM_aft_hist))
  }
}
