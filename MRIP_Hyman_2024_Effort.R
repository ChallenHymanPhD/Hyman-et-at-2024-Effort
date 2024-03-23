test = F
## Hyman's test code to check viability of function
if(test==T){
  species <- c("GAG", "RED GROUPER", "RED SNAPPER", "GRAY SNAPPER", "GREATER AMBERJACK", "VERMILION SNAPPER", "YELLOWTAIL SNAPPER", "GRAY TRIGGERFISH", "LANE SNAPPER", "MUTTON SNAPPER", "LESSER AMBERJACK", "ALMACO JACK", "BANDED RUDDERFISH", "HOGFISH", "GOLDEN TILEFISH", "BLUELINE TILEFISH", "GOLDFACE TILEFISH", "SCAMP", "BLACK GROUPER", "YELLOWEDGE GROUPER", "SNOWY GROUPER", "SPECKLED HIND", "YELLOWMOUTH GROUPER", "YELLOWFIN GROUPER", "WARSAW GROUPER",
               "GOLIATH GROUPER", "QUEEN SNAPPER", "BLACKFIN SNAPPER", "CUBERA SNAPPER", "SILK SNAPPER", "WATCHMAN")
  m <- c(paste("0", 1:9, sep = ""), "10", "11", "12")
  intdir = 'C:/Users/ichal/Documents/Hyman gag grouper models/MRIP data'
  common = species
  st = 12
  y = 2008
  styr = y
  endyr = 2008
  trips = 1
  dom = list(month = c(m), fl_reg = c("1", "2"))
}


MRIP.dirtrips<-function(intdir=NULL,common=NULL, st=NULL,styr=NULL,
                        endyr=NULL, trips=1,dom=NULL){
  # Gary Nelson, Massachusetts Division of Marine Fisheries
  # gary.nelson@mass.gov
  # MRIP employee helped solve an issue 
  #07_22_2022 - G Nelson coded method to extract the domain labels for better output  
  if(is.null(intdir)) stop("Need main directory location of intercept files.")
  if(is.null(common)) stop("Need Common Name for common.")
  if(is.null(st)) stop("No state code(s) was specified.")
  if(is.null(styr)) stop("Starting year is missing.")
  if(is.null(endyr)) stop("Ending year is missing.")
  if(length(trips)!=1) stop("Only one trip option can be used at a time for accurate calculations.")
  
  ## The code below has been annoteted out for disuse
  #if(length(grep("/",intdir))==1){
  #  din<-ifelse(substr(intdir,nchar(intdir),nchar(intdir)) %in% c("/"),
  #              c(paste(intdir,"int",sep="")),c(paste(intdir,"/int",sep="")))
  #}
  #if(length(grep("\\\\",intdir))==1){
  #  din<-ifelse(substr(intdir,nchar(intdir),nchar(intdir)) %in% c("\\"),
  #              c(paste(intdir,"int",sep="")),c(paste(intdir,"\\int",sep="")))
  #}
  
  reg_1 <- paste(c(123,129,29,33,37,45,5,91,113,131))                           ## Hyman edits manually specify counties in fl_reg 1
  reg_2 <- paste(c(101,103,115,15,17,21,53,71,75,81,57))                        ## Hyman edits manually specify counties in fl_reg 2
  din <- intdir
  common<-tolower(common)
  st<-as.character(st)
  styr<-as.character(styr)
  endyr<-as.character(endyr)
  month<-c(paste("0", 1:9, sep = ""), "10", "11", "12")
  wave<-as.character(c(1,2,3,4,5,6))
  dom_id<-NULL
  dataset<-NULL
  temp<-NULL
  temp1<-NULL
  rbind2<- function(input1, input2){
    if(!is.null(ncol(input1))){
      n.input1 <- ncol(input1)
      n.input2 <- ncol(input2)
      
      if (n.input2 < n.input1) {
        TF.names <- which(names(input2) %in% names(input1))         
        column.names <- names(input2[, TF.names])
      } 
      else {TF.names <- which(names(input1) %in% names(input2))
      column.names <- names(input1[, TF.names])}
      return(rbind(input1[, column.names], input2[, column.names]))
    }
    if(is.null(ncol(input1))) return(rbind(input1,input2))
  }
  for(yr in styr:endyr){
    for (j in 1:as.numeric(length(wave))){ 
      #Get catch
      wv<-wave[j] 
      t3<-read.csv(paste(din,"/","catch_",yr,wv,".csv",sep=""),
                   colClasses=c("character"),na.strings=".")
      t3<-t3[t3$ST %in% c(st),]
      names(t3)<-tolower(names(t3))
      temp<-rbind2(temp,t3)
      #get trips
      t4<-read.csv(paste(din,"/","trip_",yr,wv,".csv",sep=""),
                   colClasses=c("character"),na.strings=".")
      t4<-t4[t4$ST %in% c(st),]
      names(t4)<-tolower(names(t4))    
      temp1<-rbind2(temp1,t4)
    }
  }
  convtolow<-function(x){
    for(i in 1:ncol(x)) x[,i]<-tolower(x[,i])
    return(x)
  }
  temp<-convtolow(temp)
  temp1<-convtolow(temp1)
  temp<-temp[,c("common","strat_id","psu_id","st","id_code","sp_code",
                "claim","release","harvest","tot_len_a","wgt_a","tot_len_b1","wgt_b1","fl_reg","tot_cat",
                "wgt_ab1","tot_len","landing")]
  temp<-temp[order(temp$strat_id,temp$psu_id,temp$id_code),]
  temp1<-temp1[order(temp1$strat_id,temp1$psu_id,temp1$id_code),]
  
  dataset<-merge(temp1,temp,by.x=c("strat_id","psu_id","id_code","st"),
                 by.y=c("strat_id","psu_id","id_code","st"),all.x=FALSE,all.y=FALSE)
  dataset$common<-as.character(dataset$common)
  dataset$common<-ifelse(is.na(dataset$common),"",dataset$common)
  if(!any(dataset$common%in%common)){
    output <- c(y, 1, 12, 7, NA, NA, 1, common, 0, 0, 100)
    names(output) <- c("year", 
                       "wave", 
                       "st", 
                       "sub_reg", 
                       "mode_fx", 
                       "area_x", 
                       "month",
                       "species",
                       "Trips",
                       "SE",
                       "PSE")
    output <- as.data.frame(output)%>%t()
    return(output)
  } else{
    if(!any(colnames(dataset) == "fl_reg")){                                    ## Hyman edits create fl_reg column for regions before 2013
      dataset$fl_reg <- as.numeric(dataset$fl_reg.x)
    }
    dataset$fl_reg[which(dataset$cnty %in% c(reg_1))] <- 1                      ## Hyman edits to ensure counties are correctly places in FL_reg 1
    dataset$fl_reg[which(dataset$cnty %in% c(reg_2))] <- 2                      ## Hyman edits to ensure counties are correctly places in FL_reg 2
    #Construct Domain
    dom_ids<-NULL
    mainlev<-length(dom)
    if(length(dom)>0){
      for(l in 1:mainlev){
        if (!any(names(dom)[l]==names(dataset))) stop(paste("Variable ",names(dom[l]),"not found in MRIP dataset"))
        if (any(names(dom)[l]==names(dataset))){
          dataset[,ncol(dataset)+1]<-"DELETE"
          names(dataset)[ncol(dataset)]<-c(paste(names(dom)[l],"1",sep=""))
          colpos<-which(names(dataset)==names(dom[l]))
          sublev<-length(dom[[l]])
          for (k in 1:sublev) dataset[,ncol(dataset)]<-ifelse(dataset[,colpos] %in% c(as.character(dom[[l]][[k]])),c(paste(names(dom)[l],k,sep="")),dataset[,ncol(dataset)])  
          dom_ids<-c(dom_ids,names(dom[l]))   
        }
      }
      test<-c("year","wave","st","sub_reg","mode_fx","area_x")
      for(gg in 1:as.numeric(length(dom_ids))){
        if(!any(dom_ids[gg]==test)) test[as.numeric(length(test)+1)]<-c(paste(dom_ids[gg],"1",sep="")) 
        if(any(dom_ids[gg]==test)){
          colpos<-which(test==dom_ids[gg])
          test[colpos]<-c(paste(dom_ids[gg],"1",sep=""))
        }
      }
      ## add Non-domain names to values in columns
      for(gg in 1:as.numeric(length(test))){
        if(substr(test[gg],nchar(test[gg]),nchar(test[gg]))!="1"){
          eval(parse(text=paste("dataset$",test[gg],'<-paste("',as.character(test[gg]),'",dataset$',test[gg],',sep="")',sep="")))
        }
      }
      
      for(hh in 1:as.numeric(length(test))){
        if(hh==1) texter<-paste("dataset$",test[hh],sep="") 
        if(hh>1) texter<-paste(texter,",","dataset$",test[hh],sep="")  
      }
      eval(parse(text=paste("dataset$dom_id<-c(paste(",texter,",sep=''))",sep="")))
      tempdom<-as.character(texter)
    }# dom>0 
    
    
    dataset$tot_cat<-as.numeric(dataset$tot_cat)
    dataset$landing<-as.numeric(dataset$landing)
    dataset$claim<-as.numeric(dataset$claim)
    dataset$harvest<-as.numeric(dataset$harvest)
    dataset$release<-as.numeric(dataset$release)
    dataset$wgt_ab1<-as.numeric(dataset$wgt_ab1)
    dataset$wp_int<-as.numeric(dataset$wp_int)
    dataset$dcomm<-dataset$common
    dataset$dtotcat<-ifelse(dataset$common%in%common,dataset$tot_cat,0)
    dataset$dlandings<-ifelse(dataset$common%in%common,dataset$landing,0)
    dataset$dclaim<-ifelse(dataset$common%in%common,dataset$claim,0)
    dataset$dharvest<-ifelse(dataset$common%in%common,dataset$harvest,0)
    dataset$drelease<-ifelse(dataset$common%in%common,dataset$release,0)
    dataset$dwgt_ab1<-ifelse(dataset$common%in%common,dataset$wgt_ab1,0)        ## The line below is Hyman's augmentation to extract only reef-fish effort
    temlab=paste("dataset$prim1_common%in%common | dataset$prim2_common%in%common | dataset$dclaim > 0 | dataset$dharvest > 0",sep="")
    
    texter1<-paste("ifelse(",temlab,",","dataset$dom_id,2)",sep="")
    eval(parse(text=paste("dataset$dom_id<-",texter1,sep="")))
    dataset$party[which(dataset$party =="")] <- "1"
    dataset$party <- as.numeric(dataset$party)
    dataset1<-aggregate(cbind(dataset$dtotcat,dataset$dlandings,dataset$dclaim,dataset$dharvest,
                              dataset$drelease,dataset$dwgt_ab1, dataset$party)
                        ,list(dataset$strat_id,
                              dataset$psu_id,dataset$id_code,dataset$wp_int,dataset$prim1_common,dataset$prim2_common,dataset$dcomm,dataset$dom_id),sum)
    names(dataset1)<-c("strat_id","psu_id","id_code","wp_int","prim1_common","prim2_common","common","dom_id_add","total.catch",
                       "harvest.A.B1","claim.A","reported.B1","released.B2","weight", "party")
    dataset1$dtrip<-1
    dfpc<-svydesign(ids=~psu_id,strata=~strat_id,
                    weights=~wp_int,nest=TRUE,data=dataset1)
    options(survey.lonely.psu = "certainty")
    results<-svyby(~party,
                   ~dom_id_add,dfpc,svytotal,vartype=c("se","cv"),keep.names=FALSE) 
    names(results)<-c("Domain","Trips","SE","PSE")
    if(length(grep("DELETE",results$Domain,fixed=TRUE))>0){
      results<-results[-c(grep("DELETE",results$Domain,fixed=TRUE)),]
    }
    results<-results[results$Domain!="2",]
    results<-results[order(results$Domain),]
    
    if(nrow(results)>0){  
      #Make more palatable Domain labels
      tempdom<-gsub("([0-9]+).*$", "",unlist(strsplit(gsub('dataset$', '', tempdom,fixed=TRUE),",")))
      for(i in 1:length(tempdom)){
        eval(parse(text=paste(tempdom[i],"_first_pos<-regexpr(pattern='",tempdom[i],"',results$Domain)",sep="")))
        eval(parse(text=paste(tempdom[i],"_label<-substr(results$Domain,start=",tempdom[i],"_first_pos,stop=attributes(",tempdom[i],
                              "_first_pos)$match.length+",tempdom[i],"_first_pos-1)",sep="")))
        eval(parse(text=paste(tempdom[i],"_first_pos<-regexpr(pattern='",tempdom[i],"',results$Domain)",sep="")))
        eval(parse(text=paste(tempdom[i],"_label<-substr(results$Domain,start=",tempdom[i],"_first_pos,stop=attributes(",tempdom[i],
                              "_first_pos)$match.length+",tempdom[i],"_first_pos-1)",sep="")))
      }
      for(i in 1:c(length(tempdom)-1)) eval(parse(text=paste(tempdom[i],"_value<-substr(results$Domain,start=attributes(",tempdom[i],
                                                             "_first_pos)$match.length+",tempdom[i],"_first_pos,stop=",tempdom[i+1],"_first_pos-1)",sep="")))
      
      #last element
      foo<-gregexpr("([[:digit:]]+)",results$Domain)
      start_positions<-NULL
      length_last_numeric<-NULL
      for(y in 1: length(foo)){
        endval<-length(foo[[y]]) #number of numbers in domain
        start_positions<-c(start_positions,foo[[y]][endval]) # last start position of numeric in string
        length_last_numeric<-c(length_last_numeric,attributes(foo[[y]])$match.length[endval]) # length of last number
      }
      last_pos<-start_positions+length_last_numeric-1
      eval(parse(text=paste(tempdom[length(tempdom)],"_value<-substr(results$Domain,start=start_positions,stop=last_pos)",sep="")))
      
      #species names
      eval(parse(text=paste("foo1<-data.frame(",tempdom[1],"=",tempdom[1],"_value)",sep="")))
      for(i in 2:length(tempdom)) eval(parse(text=paste("foo1$",tempdom[i],"<-",tempdom[i],"_value",sep="")))
      #foo1$species<-common
      outpt<-cbind(foo1,results[,-1])
      outpt$PSE<-round(outpt$PSE*100,1)
      return(outpt)
    }
    if(nrow(results)==0) return(results)
  }
}
