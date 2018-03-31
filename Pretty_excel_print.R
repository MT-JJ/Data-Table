Pretty_excel_print<-function(stat_dt,grouping_col=NULL,stat_col=NULL,var_col=NULL,grouped_col=NULL,filename=NULL,sheetname=NULL,append=F,verbose=T,...){
  require(data.table)
  require(openxlsx)
  '%notin%'<-Negate('%in%')
  #Underlying logic is prepare data.table and then excel formatting based on columns to perform row merging and shading.
  X<-suppressWarnings(data.table(stat_dt)[,names(which(sapply(stat_dt,is.factor)==T)):=lapply(.SD,function(i) as.character(i)),.SDcols=names(which(sapply(stat_dt,is.factor)==T))])
  col_pattern=NULL
  ###Need to first prepare the data.table for proper printing. Logic is whether grouped_col and var_col are present and so we can do a big switch statement (really else if) and then another between grouping_col and stat_col within this loop.
#Var_col and whether there is column shading indicator for code readability
  Var_colshade_ind<-switch(as.character(interaction(is.null(var_col),is.null(grouped_col))),
                           "TRUE.TRUE"="Neither",
                           "TRUE.FALSE"="Col_shade_no_var",
                           "FALSE.TRUE"="Var_no_colshade",
                           "Var_colshade")
  Stat_group_ind<-as.character(interaction(is.null(grouping_col),is.null(stat_col)))
  if(Var_colshade_ind=="Neither"){
    switch(Stat_group_ind,
           "FALSE.FALSE"={
             ll<-lapply(as.list(match.call())[c("stat_dt","grouping_col","stat_col")],as.name)
             nvar<-length(colnames(X)[-match(c(grouping_col,stat_col),colnames(X))])
             group_pattern<-X[,rle(eval(ll$grouping_col))$length]
             stat_pattern<-X[,rle(eval(ll$stat_col))$length]
             group_var<-c(grouping_col,stat_col)[which(list(mean(group_pattern),mean(stat_pattern))!=1)]
             non_group_var<-c(grouping_col,stat_col)[which(list(mean(group_pattern),mean(stat_pattern))==1)]
             X<-setcolorder(copy(X),c(group_var,non_group_var,colnames(X)[-match(c(group_var,non_group_var),colnames(X))]))
             row_ind<-"Inner"},
           stop("Insufficient information to group columns. Either use the 'Create Excel' function to print as is or enter the number of columns in each group."))
    } else if(Var_colshade_ind=="Var_no_colshade"){
      switch(Stat_group_ind,
             "FALSE.FALSE"={
               ll<-lapply(as.list(match.call())[c("stat_dt","grouping_col","stat_col","var_col")],as.name)
               nvar<-X[,length(rle(eval(ll$var_col))$values)]
               nvar_rep<-X[,unique(rle(eval(ll$var_col))$lengths)]
               group_pattern<-X[,rle(eval(ll$grouping_col))$length]
               stat_pattern<-X[,rle(eval(ll$stat_col))$length]
               group_var<-c(grouping_col,stat_col)[which(list(mean(group_pattern),mean(stat_pattern))!=1)]
               non_group_var<-c(grouping_col,stat_col)[which(list(mean(group_pattern),mean(stat_pattern))==1)]
               X<-setcolorder(copy(X),c(var_col,group_var,non_group_var,colnames(X)[-match(c(var_col,group_var,non_group_var),colnames(X))]))
               row_ind<-"Outer"},
             "TRUE.TRUE"={
               stop("Insufficient information to group columns. Either use the 'Create Excel' function to print as is or enter the number of columns in each group.")},
             #Default for switch is either stat or var
             {ll<-lapply(as.list(match.call())[c("stat_dt","var_col")],as.name)
             nvar<-X[,length(rle(eval(ll$var_col))$values)]
             nvar_rep<-X[,unique(rle(eval(ll$var_col))$lengths)]
             if(!is.null(stat_col)){
               ll<-append(ll,list(stat_col=as.name(stat_col)))
               stat_pattern<-X[rle(eval(ll$stat_col))$lengths]
               if(nvar_rep!=1){
                 group_var<-var_col
                 non_group_var<-stat_col
                 } else {
                   group_var<-stat_col
                   non_group_var<-var_col
                 }
               } else{
                 ll<-append(ll,list(grouping_col=as.name(grouping_col)))
                 group_pattern<-X[,rle(eval(ll$grouping_col))$length]
                 if(nvar_rep!=1){
                   group_var<-var_col
                   non_group_var<-grouping_col
                   } else {
                     group_var<-grouping_col
                     non_group_var<-var_col
                   }
                 }
             X<-setcolorder(copy(X),c(group_var,non_group_var,colnames(X)[-match(c(group_var,non_group_var),colnames(X))]))
             col_pattern<-seq(3,ncol(X),by=grouped_col)
             row_ind<-"Inner"
             })
      } else {##Colshade_no_var
        switch(Stat_group_ind,
               "FALSE.FALSE"={
                 ll<-lapply(as.list(match.call())[c("stat_dt","grouping_col","stat_col")],as.name)
                 group_pattern<-X[,rle(eval(ll$grouping_col))$length]
                 stat_pattern<-X[,rle(eval(ll$stat_col))$length]
                 group_var<-c(grouping_col,stat_col)[which(list(mean(group_pattern),mean(stat_pattern))!=1)]
                 non_group_var<-c(grouping_col,stat_col)[which(list(mean(group_pattern),mean(stat_pattern))==1)]
                 X<-setcolorder(copy(X),c(group_var,non_group_var,colnames(X)[-match(c(group_var,non_group_var),colnames(X))]))
                 col_pattern<-seq(3,ncol(X),by=grouped_col)
                 row_ind<-"Inner"},
               "TRUE.TRUE"={
                 col_pattern<-seq(2,ncol(X),by=grouped_col)
                 row_ind<-"None"},
               {#Default for switch is one or the other and so only need the first variable anyway so  no need for anything else.
                 col_pattern<-seq(2,ncol(X),by=grouped_col)
                 row_ind<-"None"})
        }
  ####Starting to create workbook
  #####Style parameters
  header_style<-createStyle(textDecoration = "bold",fontSize = 14,halign = "center",fgFill = "azure3",border = "TopBottom",borderColour = "black",borderStyle = "thick")
  ##Creating Shading and Centering Styles and borders
  shade<-createStyle(fgFill = "azure2")
  font<-createStyle(fontSize = 12)
  outer_shade<-createStyle(fgFill = "azure3",textDecoration = "bold",fontSize = 14)
  center_cell<-createStyle(halign = "center")
  merge_center<-createStyle(valign = "center",halign = "center")
  bottom_border<-createStyle(border = "bottom",borderStyle = "thick")
  top_border<-createStyle(border = "top",borderStyle = "thick")
  col_border<-createStyle(border="left",borderStyle = "medium",borderColour = "blue")
  if(is.null(sheetname)){
    sheetname<-as.character(match.call()$stat_dt)
  }
  if(is.null(filename)){filename<-paste0(c(sheetname,"xlsx"),collapse = ".")}
  if(nchar(sheetname)>=31){sheetname<-substr(sheetname,1,30)}#saves space to add if duplicate sheetnames under 10
  ###Creating Workbook
  if(append==F){
    wb<-createWorkbook()
    } else {
      if(verbose){
        print(paste0("Appending output to ",filename," at",getwd()))
        }
      wb<-loadWorkbook(paste0(c(getwd(),filename),collapse="/"))
      if(suppressWarnings(!is.na(match(names(wb),sheetname)))){
        base_name<-sheetname
        all_basenames<-grep(base_name,names(wb),value=T)
        sheetname<-paste0(base_name,"_",length(all_basenames)+1)
        if(suppressWarnings(!is.na(match(names(wb),sheetname)))){
          sheetname<-paste0(base_name,"_",length(all_basenames)+2)
        }
      }
      }
  addWorksheet(wb,sheetname,gridLines = F,orientation = "landscape")
  writeData(wb,sheetname,X,borders = "surrounding",borderStyle = "thick",borderColour = "black")
  ###Header
  invisible(addStyle(wb,sheetname,style = header_style,rows = 1,cols = seq_along(X)))
  invisible(addStyle(wb,sheetname,style = createStyle(border = "right",borderStyle = "thick",borderColour = "black"),rows = 1,cols=ncol(X),stack = T))
  ###Centering data cells and enlarging font size
  invisible(addStyle(wb,sheetname,style = font,cols=seq_along(X)[-1],rows=X[,seq_along(.I)]+1,gridExpand = T,stack = T))
  #########Creating vectors for row shading and merging
  #Function to generate sequences by recursively adding the rle patterns of grouping variables
  seq_fn<-function(starting,rle_vector){
    a<-starting
    x<-rle_vector
    for(i in seq_along(x)[-length(x)]){
      a<-c(a,a[i]+x[i])
      }
    return(a)
    }
  switch(row_ind,
         "Outer"={
           outer_pattern<-rle(X[[1]])$lengths
           startMergeStat<-seq_fn(2,outer_pattern)
           inner_pattern<-rle(X[[2]])$lengths
           startMergeGroup<-seq_fn(2,inner_pattern)
           MergeStatrow<-lapply(seq_along(startMergeStat),function(i) seq(startMergeStat[i],length.out = outer_pattern[i]))
           MergeGrouprow<-lapply(seq_along(startMergeGroup),function(i) seq(startMergeGroup[i],length.out = inner_pattern[i]))
           invisible(addStyle(wb,sheetname,style = center_cell,cols=seq_along(X)[-c(1:3)],rows=X[,seq_along(.I)]+1,gridExpand = T,stack = T))
           ##Shading inner grouping column Rows by alternating
           shade_rows<-MergeGrouprow[seq(1,length(MergeGrouprow),by=2)]
           invisible(lapply(seq_along(MergeStatrow),function(i) addStyle(wb,sheetname,style=shade,cols=seq_along(readWorkbook(wb,sheetname))[-1],rows=shade_rows[[i]],gridExpand = T,stack = T)))
           ##Merging Cells
           #Outer
           invisible(lapply(seq_along(MergeStatrow),function(i) mergeCells(wb,sheetname,cols=1,rows=MergeStatrow[[i]])))
           #Inner
           invisible(lapply(seq_along(MergeGrouprow),function(i) mergeCells(wb,sheetname,cols=2,rows=MergeGrouprow[[i]])))
           ##Centering inner and outer
           invisible(addStyle(wb,sheetname,style=merge_center,cols=1:2,rows=c(1,X[,seq_along(.I)]+1),gridExpand = T,stack = T))
           ####Adding Borders around outer and the bottom of table
           invisible(addStyle(wb,sheetname,style=top_border,cols = seq_along(X),rows = startMergeStat,gridExpand = T,stack = T))
           invisible(addStyle(wb,sheetname,style=bottom_border,cols = seq_along(X),rows = nrow(X)+1,gridExpand = T,stack = T))
           },
         "Inner"={
           inner_pattern<-rle(X[[1]])$lengths
           startMergeGroup<-seq_fn(2,inner_pattern)
           MergeGrouprow<-lapply(seq_along(startMergeGroup),function(i) seq(startMergeGroup[i],length.out = inner_pattern[i]))
           invisible(addStyle(wb,sheetname,style = center_cell,cols=seq_along(X)[-c(1:2)],rows=X[,seq_along(.I)]+1,gridExpand = T,stack = T))
           border_rows<-MergeGrouprow[seq(1,length(MergeGrouprow),by=2)]
           ##Merging and Centering Inner Cells
           invisible(lapply(seq_along(MergeGrouprow),function(i) mergeCells(wb,sheetname,cols=1,rows=MergeGrouprow[[i]])))
           invisible(addStyle(wb,sheetname,style=merge_center,cols=1:2,rows=c(1,X[,seq_along(.I)]+1),gridExpand = T,stack = T))
           ##Adding border at bottom of table
           invisible(addStyle(wb,sheetname,style=bottom_border,cols = seq_along(X),rows = nrow(X)+1,gridExpand = T,stack = T))
           },
         "None"={
           if(verbose){
             print("No grouping by rows indicated defaulting to alternating row shading.")
             }
           invisible(addStyle(wb,sheetname,style = center_cell,cols=seq_along(X)[-1],rows=X[,seq_along(.I)]+1,gridExpand = T,stack = T))
           invisible(addStyle(wb,sheetname,style = shade,cols = seq_along(X)[-1],rows = seq(2,nrow(X)+1,by=2),gridExpand = T,stack = T))
           })
  ###Setting Column Widths to Auto and also assuring fint of first column is bold with larger font.
  ##Shading Outer Column and centering
  invisible(addStyle(wb,sheetname,style=outer_shade,cols=1,rows = c(1,X[,seq_along(.I)]+1),stack = T))
  invisible(setColWidths(wb,sheetname,cols=seq_along(X),widths = "auto"))
  ###Adding column borders if desired
  if(!is.null(col_pattern)){
    invisible(addStyle(wb,sheetname,style=col_border,cols = col_pattern,rows=seq(1,nrow(X)+1),gridExpand = T,stack = T))
    }
  if(verbose){
    print(paste("Saving",sQuote(filename),"to",getwd()))
    }
  saveWorkbook(wb,filename,overwrite = T)
}


