# Here, assuming we only deal with daily, non-comparable SVI
search_terms = c('euromicron',	'TC Unterhaltungstechnik',	'SGL Carbon',	'zooplus',	'TUI',	'Borussia Dortmund',	'EUCA',	'TCU',	'SGL',	'ZO1',	'TUI1',	'BVB')
frequency = 'daily'
comparable = TRUE
country = NA
region = NA 
year = NA

years = c(2004,2005,2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
months = c(1,4,7,10)
length = 3  

url=vector()
counter = 1
for(search_term in search_terms){
  for(year in years){
    for(month in months){
      if(year == as.numeric(substr(as.character(Sys.time()), 1, 4)) & month > as.numeric(substr(as.character(Sys.time()), 6, 7))){
        next() #  This stops us from creating URLs for dates that don't exist.
      }
      url[counter]=URL_GT(keyword=search_term, year=year, month=month)
      counter = counter + 1
    }
  }
}

for(search_term in search_terms){
  url[counter]=URL_GT(search_term)
  counter = counter + 1
}

for(i in 1:length(url)){
  lynx_commands <- lynx_script(url[i]) #  Create the lynx script
  write.table(lynx_commands, '/root/gt_download', row.names=F, col.names=F, quote=F) #  Save the lynx script
  system("lynx -cmd_script=/root/gt_download www.google.com") #  Execute the lynx script (takes a while, be patient)
}
