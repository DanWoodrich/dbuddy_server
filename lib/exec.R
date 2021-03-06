standardwait<-function(path,bool){
  
  s=0
  while(file.exists(path)==bool){
    
    if(s<10){
      Sys.sleep(1)
      s=s+1
    }else if(s<100){
      Sys.sleep(5)
      s=s+5
    }else if(s<1000){
      Sys.sleep(60)
      s=s+60
    }
    
    print(paste("waiting for response: ",s,"s",sep=""))
    
  }
  
}

comppath = Sys.getenv("COMPATH")

#comppath = "\"//161.55.120.117/NMML_AcousticsData/Working_Folders/test_db\""
comppath = substr(comppath,2,nchar(comppath)-1) #this is hardcoded to work with NAS paths, like above

responsedir = paste(comppath,"/requests",sep="")
#comppath="//161.55.120.117/NMML_AcousticsData/Working_Folders/test database"

db_fp = Sys.getenv("DBPATH")

s=0.5
backup_num = 0
IPID="Unassigned"
#now, wait for request file and inform user of waiting:
serve<-function(s){
  
  while(length(dir(responsedir))==0){
    
    if(s%%2==0){
      print(paste("waiting... uptime (days):",(s/(3600*24))))
    }
    
    if(s%%259200==0){ #every 3 days of uptime, back up the db
      
      backup_num=backup_num+1
      
      db_new_name = paste(substr(basename(db_fp),1,nchar(basename(db_fp))-3),backup_num,".db",sep="")
      
      file.copy(db_fp,paste("//161.55.120.117/NMML_AcousticsData/Working_Folders/test_db/backups",db_new_name,sep="/"))
      
      if(backup_num==5){ #reset after 5 files. 
        backup_num=0
     }
      
    }
    
    s = s+0.25
    #print(s)
    Sys.sleep(0.25)
  }
  
  print("request recieved!")
  #break loop if file available:
  requests = dir(responsedir)
  
  #extract IPID- process these together first- do later
  #reqIP_IDS = substr(requests)
  
  #for now, just do this first
  request = requests[1]
  

  file = paste(responsedir,request,sep="/")
  
  args = readLines(file)

  file.remove(file)

  args = strsplit(args," ")[[1]]

  #delete request file 
  
  
  #before running the command, I need to inject the NAS outpath as the intended outpath. 
  
  if(args[1]=='pull'|args[1]=='pull_from_data'|("--out" %in% args)){
    
    #substitude nas_outfile into args
    nas_outfile = paste(comppath,"/output/",substr(request,1,nchar(request)-4),".csv",sep="")
    if(args[1]=='pull'){
      args[3] = nas_outfile
    }else if(args[1]=='pull_from_data'){
	  args[4] = nas_outfile
	}else{
      args[which(args=="--out")+1]= nas_outfile
    }
    
  }else{
    nas_outfile = paste(comppath,"/output/",request,sep="")
  }
  
  #command = paste(c("dbuddy",args),collapse = " ")
  #print(command)
  
  #print(args)
  #log = system(command,intern=T)
  badsymbols = c("*","<",">")
  if(any(args %in% badsymbols)){
    badsymbols = badsymbols[which(badsymbols %in% args)]
    for(n in 1:length(badsymbols)){
	  if(badsymbols[n]=="<"){
	    args[which(args==badsymbols[n])]<- 'lThan'
      }else if(badsymbols[n]==">"){
	    args[which(args==badsymbols[n])]<- 'gThan'
      }else{
      args[which(args==badsymbols[n])]<- paste("'",badsymbols[n],"'",sep="")
	  }
    }
  }
  
  print(args)
  
  #print(args)
  
  if(args[1]!='ping'){
    args= paste(args,collapse=" ")
	#print(args)
    log <- system2("dbuddy",args, stdout=TRUE, stderr=TRUE)
	
	#print(log)
    
  }else{
    log<-paste("server online","\nuptime",s/3600/24,"days","\n",R.version$version.string)
  }
  
  
  #print(args)
  
  #print(log)
  
  #if file not generated, as in the case of pulls and DML errors, save the log as the output file. 
  tryCatch(
    {
        invisible(readLines(con = nas_outfile, n = 1))
    },
    error = function(e){
        writeLines(log, paste(comppath,"/output/",request,sep=""))
    }
  ) 
  
  #write the log as the output file. Or, write the output as the output file. 
  
  print("request complete")
  serve(s) #recursion to keep this party going 
}
  
serve(s)

