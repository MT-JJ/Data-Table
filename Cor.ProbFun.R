cor.prob<-function (X,use="complete.obs",method="pearson",alternative="two.sided") {
  require(data.table)
  require("Matrix")
  '%notin%'<-Negate('%in%')
  namesc<-colnames(X)
  n=ncol(X)
  test_list<-lapply(lapply(combn(namesc,2,simplify = F),function(i) paste0("~",paste0(i,collapse="+"))),function(j) cor.test(as.formula(j),data=X,method = method,alternative = alternative))
  UniqueR<-round(na.omit(matrix(t(car::recode(upper.tri(cor(X)),"'FALSE'=NA")*cor(X,use=use,method=method)))),digits=4)
  
  UniqueRsig<-data.table(data.frame(Corr=UniqueR,p_val=unlist(lapply(test_list,"[[","p.value")),Corr_form=format(UniqueR,nsmall=4)))[,Corr_sig:=paste0(Corr_form,ifelse(abs(p_val) < .001, "***", ifelse(abs(p_val) < .01, "** ", ifelse(abs(p_val) < .05, "* ", " "))))][,p_val:=format(round(p_val,digits=4),nsmall=4)][,Test_name:=unlist(lapply(test_list,"[[","data.name"))]
  
  test_fn<-function(X){
    starting<-list(X)
    Y<-UniqueRsig$Corr_sig
    Z<-UniqueRsig$p_val
    listnames<-UniqueRsig$Test_name
    for(i in seq_along(X)){
      starting[[i]]<-rbind(Test=listnames[1:X[i]],Corr_sig=Y[1:X[i]],p_val=Z[1:X[i]])
      Y<-Y[-c(1:X[i])]
      Z<-Z[-c(1:X[i])]
      listnames<-listnames[-c(1:X[i])]
    }
    return(starting)
  }
  
  ragged_corlist<-unlist(list(lapply(test_fn((n-1):0),function(i) unlist(melt(data.table(i,keep.rownames = T),id.vars="rn")[rn!="Test",value])),list(character(0))),recursive = F)[-n]
  
  even_corlist<-setcolorder(data.table(data.frame(setattr(lapply(seq_along(namesc),function(i) c(rep(c(NA,1,NA),c(2*(i-1),1,1)),ragged_corlist[[i]])),"names",namesc)))[,Items:=rapply(lapply(namesc,function(i) c(i,"p_value")),c)],c("Items",namesc))
  return(even_corlist)
}
