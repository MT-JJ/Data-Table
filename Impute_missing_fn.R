Impute_missing_fn<-function(dataX,impute_method="Rosenbaum",ignore_vars=NULL,verbose=T){
  require(data.table)
  dataframe_name<-as.character(pairlist_converter(as.list(match.call())$dataX))
  if(!is.null(ignore_vars)){
    X<-data.table(dataX)[,-ignore_vars,with=F]
    ignore_vars<-data.table(dataX)[,ignore_vars,with=F]
  } else {X<-data.table(dataX)}
  Missing_names<-names(which(X[,sapply(copy(.SD),function(i) sum(is.na(i)))]!=0))
  if(length(Missing_names)>0){
    Missing_vars<-data.table(X)[,Missing_names,with=F][,lapply(copy(.SD),function(i) if(is.character(i)|is.factor(i)){as.character(i)} else{as.numeric(i)})]
    Missing_factor_names<-names(which(sapply(Missing_vars,is.character)))
    ########Start of Switch#######
    switch(toupper(substr(impute_method,1,1)),
           "R"={#####Rosenbaum's Method#######
             Missing_numeric_values<-tryCatch(Missing_vars[,which(sapply(Missing_vars,is.numeric)),with=F][,sapply(copy(.SD),function(i) ceiling(max(range(i,na.rm=T)))+5)],error=function(e) NULL)##Get numeric values
             ##Control flow for numeric vs factor variables
             if(length(Missing_numeric_values)>0){
               Missing_vars[,names(Missing_numeric_values):=lapply(seq_along(copy(.SD)),function(i) ifelse(is.na(copy(.SD)[[i]]),Missing_numeric_values[i],copy(.SD)[[i]])),.SDcols=names(Missing_numeric_values)]##Assigning numeric values to original variables
               Missing_vars[,paste(names(Missing_numeric_values),"missing",sep="_"):=lapply(names(Missing_numeric_values),function(i) ifelse(Missing_vars[[i]]==Missing_numeric_values[i],1,0))]##Creating missing indicator variables for numeric data.
               if(length(Missing_factor_names)>0){
                 Missing_vars[,eval(Missing_factor_names):=lapply(.SD,function(i) ifelse(is.na(i),"missing",i)),.SDcols=Missing_factor_names]##Assigining missing to factors
               }
             } else {##Only factor missing
               Missing_vars[,eval(Missing_factor_names):=lapply(.SD,function(i) ifelse(is.na(i),"missing",i)),.SDcols=Missing_factor_names]##Assigining missing to factors
             }
           },
           {
             print("Hi")
           })
    ########End of Switch#######
  } else {
    if(verbose){
      print("You indicated missing check but no variables were missing.")
      return(dataX)
    }
  }
  ######Control Flow for whether there were variables to ignore#######
  if(!is.null(ignore_vars)){
    X<-suppressWarnings(cbind(X[,-Missing_names,with=F],Missing_vars,ignore_vars))
  } else {
    X<-suppressWarnings(cbind(X[,-Missing_names,with=F],Missing_vars))
  }
}
