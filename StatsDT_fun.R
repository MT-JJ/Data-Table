StatsDT_fn<-function(X,groupvars,outcome_vars=NULL,ID=NULL,stat_col=T,group_col=T,varcol=T,nlevels_cont=10,round_digit=2,percents=T,verbose=T){
  require(data.table)
  '%notin%'<-Negate('%in%')
  if(verbose){
    print(paste("Assuming",sQuote(groupvars[1]),"is the treatment variable. If not please re-enter your grouping variables with the treatment variable as the first variable."))
    }
  if(group_col!=F){
    if(length(groupvars)>1){
      ll<-list(groupvars=unlist(strsplit(gsub("^c|[[:punct:]]","",deparse(match.call()$"groupvars")),split = " ")))
      mult_group_vars=T
      if(group_col==T){
        stop("You entered multiple grouping variables an indicated you wanted them in columns so you need to specify which of them to include in columns. Re-run the function by replacing the 'T' in 'group_col' with a character vector of variable names or 'A' if you want all of them in columns.")
      }
      if(group_col=="A") {group_col<-groupvars}
      } else {mult_group_vars=F}
    } else {mult_group_vars=F} #This is case where group_col=F which overrides the multiple variables in groupvars since they won't be printed in columns anyway.
#This changes value in group_col to name of the single group variable in groupvars for consistency later.
  if(group_col==T&mult_group_vars==F){
    ll<-lapply(as.list(match.call())["groupvars"],as.name)
    group_col<-groupvars
    }
  #Creates X without ID variable and identifies outcome variables
  if(is.null(outcome_vars)){
    if(is.null(ID)){
      outcome_vars<-colnames(X)[-match(groupvars,colnames(X))]
      } else {
        outcome_vars<-colnames(X)[-match(c(groupvars,ID),colnames(X))]
        X<-X[,-ID,with=F]
        if(.Platform$OS.type=="unix"){
          if(sys.nframe()>1){
            orig_id<-orig_call_fn(match.call()$ID)
            }
          if(verbose){
            print(paste("Assuming ID variable is", sQuote(orig_id),"and removing it from descriptive statistics."))
            }
          }##Closes operating system indicator
        }##Closes ID detector loop
  } else{
      if(!is.null(ID)){ X<-X[,-ID,with=F]}
    }
  X<-X[,c(groupvars,outcome_vars),with=F]
  #Identifying which variables are continuous for adding percents later.
  continuous_vars<-names(which(sapply(X,function(i) length(unique(i))>nlevels_cont)==T))
  #Checking for missing values in groupvars and recoding them to unknown as necessary
  grouplist_test<-copy(X)[,groupvars,with=F]
  invisible(lapply(seq_along(grouplist_test),function(i) if(sum(is.na(grouplist_test[[i]]))>1){
    if(verbose){
      print(paste("WARNING:",groupvars[i],"has",sum(is.na(grouplist_test[[i]])), "missing values. Coding them to Unknown."))
      }
    X[,groupvars[i]:=lapply(.SD,function(x) as.character(ifelse(is.na(X[[x]]),"Unknown",X[[x]]))),.SDcols=groupvars[i]]
    }))
  ##Calculating stats by interacting the group variables
  grouplist<-lapply(groupvars,function(i) as.character(X[[i]]))
  Mod_X<-copy(X)[,Group:=interaction(grouplist)]
  
  results_list<-setattr(lapply(outcome_vars,function(j) Mod_X[is.na(Mod_X[[j]])==F,lapply(.SD,function(i) list(N=.N,Mean=format(round(mean(i,na.rm=T),round_digit),nsmall=round_digit),SD=format(round(sd(i,na.rm=T),round_digit),nsmall=round_digit))),by=groupvars,.SDcols=j][,Stat:=rep(c("N","Mean","SD"),length(unique(interaction(grouplist))))]),"names",outcome_vars)
  
  raw_results<-Reduce(function(...) merge(...,by=c(groupvars,"Stat"),all=T), results_list)[,lapply(.SD,as.character)][,Stat:=factor(Stat,levels = c("N","Mean","SD"))]
  if(percents==T){
    percent_cols<-colnames(raw_results)[-match(c(groupvars,"Stat",continuous_vars),colnames(raw_results))]
    raw_results<-copy(raw_results)[Stat=="Mean",eval(percent_cols):=lapply(.SD,function(i) paste0(as.character(round(as.numeric(i)*100,0)),"%")),.SDcols=eval(percent_cols)]
    }
  ####Creating the casting layout for final results starting with whether there are multiple grouping variables and then whether user wants a variable column or if they should be spread in a wide format.
  stat_var_ind<-interaction(varcol,stat_col)
  recast<-NULL
  #Group_col can only take on False and char string
  if(group_col==F){
    switch(as.character(stat_var_ind),
           "TRUE.TRUE"={
             dcast_formula<-paste("variable","+","Stat","~",paste0(groupvars,collapse = "+"))},
           "TRUE.FALSE"={
             dcast_formula<-paste("variable","~",paste0(c("Stat",groupvars),collapse = "+"))},
           "FALSE.TRUE"={
             dcast_formula<-paste("Stat","~",paste0(c("variable",groupvars),collapse = "+"))},
           {stop("Need at least one column")})
    } else {
      switch(as.character(stat_var_ind),
             "FALSE.FALSE"={
               dcast_formula<-paste(paste0(group_col,collapse="+"),"~",paste0(c("variable","Stat",groupvars[-match(groupvars,group_col)]),collapse = "+"))},
             "TRUE.FALSE"={
               dcast_formula<-paste(paste0(c("variable",group_col),collapse="+"),"~",paste0(c("Stat",groupvars[-match(groupvars,group_col)]),collapse = "+"))},
             "FALSE.TRUE"={
               dcast_formula<-paste(paste0(c(group_col,"Stat"),collapse="+"),"~",paste0(c("variable",groupvars[-match(groupvars,group_col)]),collapse = "+"))},
             {recast=F})
      }
  if(is.null(recast)){
    final_results<-dcast(melt(raw_results,id.vars=c(groupvars,"Stat"),variable.factor=F),dcast_formula)
    } else {
      final_results<-melt(raw_results,id.vars=c(groupvars,"Stat"),variable.factor=F)
      }
  return(final_results)
}
