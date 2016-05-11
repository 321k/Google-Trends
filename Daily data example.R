
library(Rmisc)
library(ggplot2)
library(dplyr)

# The Google Trends formating functions -----------------------------------

#This script automates the downloading of Google Trends.
#It works best with firefox in combination with the Tab Mix Plus add-on that is used to automate tab closing.
#Ask firefox not to prompt for new downloads and this script should run automatically.
#Google Trends restricts the number of download to roughly 400 at a time.

URL_GT=function(keyword="", country=NA, region=NA, year=NA, month=1, length=3){
  
  start="http://www.google.com/trends/trendsReport?hl=en-US&q="
  end="&cmpt=q&content=1&export=1"
  geo=""
  date=""
  
  #Geographic restrictions
  if(!is.na(country)) {
    geo="&geo="
    geo=paste(geo, country, sep="")
    if(!is.na(region)) geo=paste(geo, "-", region, sep="")
  }
  
  queries=keyword[1]
  if(length(keyword)>1) {
    for(i in 2:length(keyword)){
      queries=paste(queries, "%2C ", keyword[i], sep="")
    }
  }
  
  #Dates
  if(!is.na(year)){
    date="&date="
    date=paste(date, month, "%2F", year, "%20", length, "m", sep="")
  }
  
  URL=paste(start, queries, geo, date, end, sep="")
  URL <- gsub(" ", "%20", URL)
  return(URL)
}

downloadGT=function(URL, downloadDir){
  
  #Determine if download has been completed by comparing the number of files in the download directory to the starting number
  startingFiles=list.files(downloadDir)
  browseURL(URL)
  endingFiles=list.files(downloadDir)
  
  while(length(setdiff(endingFiles,startingFiles))==0) {
    Sys.sleep(3)
    endingFiles=list.files(downloadDir)
  }
  filePath=setdiff(endingFiles,startingFiles)
  return(filePath)
}


readGT=function(filePath){
  rawFiles=list()
  
  for(i in 1:length(filePath)){
    if(length(filePath)==1) rawFiles[[1]]=read.csv(filePath, header=F, blank.lines.skip=F)
    if(length(filePath)>1) rawFiles[[i]]=read.csv(filePath[i], header=F, blank.lines.skip=F)
  }
  
  output=data.frame()
  name=vector()
  
  for(i in 1:length(rawFiles)){
    data=rawFiles[[i]]
    name=as.character(t(data[5,-1]))
    
    #Select the time series
    start=which(data[,1]=="")[1]+3
    stop=which(data[,1]=="")[2]-2
    
    #Skip to next if file is empty
    if(ncol(data)<2) next
    if(is.na(which(data[,1]=="")[2]-2)) next
    
    data=data[start:stop,]
    data[,1]=as.character(data[,1])
    
    #Convert all columns except date column into numeric
    for(j in 2:ncol(data)) data[,j]=as.numeric(as.character(data[,j]))
    
    #FORMAT DATE
    len=nchar(data[1,1])
    
    #Monthly data
    if(len==7) {
      data[,1]=as.Date(paste(data[,1], "-1", sep=""), "%Y-%m-%d")
      data[,1]=sapply(data[,1], seq, length=2, by="1 month")[2,]-1
      data[,1]=as.Date(data[,1], "%Y-%m-%d", origin="1970-01-01")
    }
    
    #Weekly data
    if(len==23){
      data[,1]=sapply(data[,1], substr, start=14, stop=30)
      data[,1]=as.Date(data[,1], "%Y-%m-%d")
    }
    
    #Daily data
    if(len==10) data[,1]=as.Date(data[,1], "%Y-%m-%d")
    
    #Structure into panel data format
    panelData=data[1:2]
    panelData[3]=name[1]
    names(panelData)=c("Date", "SVI", "Keyword")
    if(ncol(data)>2) {
      
      for(j in 3:ncol(data)) {
        appendData=data[c(1,j)]
        appendData[3]=name[j-1]
        names(appendData)=c("Date", "SVI", "Keyword")
        panelData=rbind(panelData, appendData)
      }
    }
    
    #Add file name  
    panelData[ncol(panelData)+1]=filePath[i]
    
    #Add path to filename
    names(panelData)[4]="Path"
    
    #Merge several several files into one
    if(i==1) output=panelData
    if(i>1) output=rbind(output, panelData)
  }
  return(output)
}

readGeoGT=function(filePath){
  output=data.frame()
  rawFiles=list()
  for(i in 1:length(filePath)){
    if(length(filePath)==1) rawFiles[[1]]=read.csv(filePath, header=F, blank.lines.skip=F)
    if(length(filePath)>1) rawFiles[[i]]=read.csv(filePath[i], header=F, blank.lines.skip=F)
  }
  
  for(i in 1:length(rawFiles)){
    data=rawFiles[[i]]
    start=which(data[,1]=="")[3]+3
    stop=which(data[,1]=="")[4]-1
    names=data[start-1,]
    
    for(j in 1:ncol(names)) names(data)[j]=as.character(names[1,j])
    data=data[start:stop,]
    data[,1]=as.character(data[,1])
    data[,-1]=as.numeric(as.character(data[,-1]))
    data[ncol(data)+1]=filePath[i]
    
    output=rbind(output, data)
  }
  return(output)
}


# Downloading the data ----------------------------------------------------


search_terms = c("bull market", "bear market", "recession")

years = c(2005,2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
months = c(1,4,7,10)
res.daily=list()
counter=1
for(year in years){
  for(month in months){
    url=URL_GT(search_terms, year=year, month=month)
    GT_dir = downloadGT(url, downloadDir)
    GT_dir = paste(downloadDir, GT_dir, sep='/')
    res.daily[[counter]] = readGT(GT_dir)
    counter=counter+1
  }
}

df.daily <- do.call("rbind", res.daily)

url = URL_GT(search_terms)
GT_dir = downloadGT(url, downloadDir)
GT_dir = paste(downloadDir, GT_dir, sep='/')
df.weekly = readGT(GT_dir)


# Formating the data ------------------------------------------------------


df.merged = merge(df.daily, df.weekly, by=c('Date', 'Keyword'), all.x=T)
df.merged$adjustment_factor = df.merged$SVI.y /df.merged$SVI.x

for(i in search_terms){
  r=which(df.merged$Keyword==i)
  for(j in 2:length(r)){
    if(!is.finite(df.merged$adjustment_factor[r][j])){
      df.merged$adjustment_factor[r][j] = df.merged$adjustment_factor[r][j-1]
    }
  }
}
df.merged$daily = df.merged$adjustment_factor * df.merged$SVI.x
df.merged$weekly = df.merged$SVI.y
for(i in search_terms){
  r=which(df.merged$Keyword==i)
  for(j in 2:length(r)){
    if(is.na(df.merged$weekly[r][j])){
      df.merged$weekly[r][j] = df.merged$weekly[r][j-1]
    }
  }
}


# Plotting the data -------------------------------------------------------

df.merged$daily[which(is.infinite(df.merged$daily))] = NA

p1 = df.merged %>%
  ggplot(aes(Date, daily, color=Keyword))+geom_line()

p2 = df.merged %>%
  ggplot(aes(Date, weekly, color=Keyword))+geom_line()

multiplot(p1,p2)


# Saving the data ---------------------------------------------------------


write.csv(df.merged,'df.merged.csv')
