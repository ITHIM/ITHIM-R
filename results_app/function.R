createcsv<-function(misura,aggr,data){
  #misura<-'mpg'
  #aggr<-'Sum'
  intest<-paste0(aggr,"(",misura,")")
  #a<-readr::read_lines('data.json')
  data<-jsonlite::fromJSON(data)
  ref<-data$data
  nrow<-length(ref)
    
# compute numero of col
  ncol<-0

  w<-ref["row1"][[1]]
  for(j in colnames(w)){
    ncol<-ncol+unlist(ref["row1"][[1]][j][[1]]["colSpan"])
  }
  
  ris<-list()
  for(i in 1:nrow)
    ris<-c(ris,list(replicate(ncol,"")))
  idrow<-1    
  for(i in colnames(ref)){
    #lista di liste con attributi celle
    #i<-"row1"
    attribs<-ref[i][[1]]
    
    # col<-ref[i][[1]]
    # ncolcur<-length(col)
    
    #init row span max
    idcol<-1
    
    for(j in colnames(attribs)){
      #attributi cella
      attrib<-attribs[j][[1]]
      rspan<-as.integer(attrib["rowSpan"])
      cspan<-as.integer(attrib["colSpan"])
      while(ris[[idrow]][idcol]!=""){
        idcol<-idcol+1  
      }
      
      
      for(ktmp in 1:rspan){
        for(stmp in 1:cspan){
          ris[[ktmp+idrow-1]][stmp+idcol-1]<-"#"
        }
      }
      val<-as.character(attrib["innerhtml"])
      ris[[idrow]][idcol]<-val
      idcol<-idcol+cspan  
  
      
    }
    idrow<-idrow+1
  }
  ris[[1]][1]<-intest
  risa<-lapply(ris,function(x){stringr::str_replace(x,"#","")})

  tocsv<-as.data.frame(matrix(unlist(risa),ncol=ncol,byrow=TRUE),optional=TRUE)
  createcsv<-tocsv
}
