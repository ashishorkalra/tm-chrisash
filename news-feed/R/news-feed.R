##################################################################################
## loading/installing packages
##################################################################################
load.package = function(package.name){
  package.name = as.character(substitute(package.name))
  if(require(package.name, character.only = TRUE)) { 
    message(paste(package.name, ' loaded', sep = ""))
  } else {
    install.packages(package.name)
  }
}

##################################################################################
## Load data from web or from disk if already stored
##################################################################################

load.data = function(url, type = "xml", file.name, overwrite = FALSE){
  load.package(RCurl)
  
  if(file.exists(file.name) & overwrite == FALSE){
    # print(1)
    return(readRDS(file.name))
  } else {
    # print(2)
    var = switch(type,                     #add additional data sources here
                 xml  = xmlParse(url),
                 json = getURL(url))       #check if correct! Pass apa.api urls?
    saveRDS(var, file = file.name)
    return(var)
  }
}

##################################################################################
## Convert UNIX time stamp to date object and vice versa
##################################################################################

unix2date = function(time.stamp){
  val = time.stamp
  as.POSIXct(val, origin="1970-01-01")
  as.Date(as.POSIXct(val, origin="1970-01-01"))
}

date2unix = function(year, month, day){
  #returns UNIX time: seconds since 01.01.1970 GMT
  as.double(as.POSIXct(as.Date(paste(year, month, day, sep="-")))) 
}

##################################################################################
## Build APA query
##################################################################################
# For example see: http://api.ots.at/wizard/ and
# http://api.ots.at/doku/  for more details

apa.query = function(AppID, channel="", content="alle", 
                     from = 1422745200, to = 1422745200, 
                     number = 10, format = "json")
{ 
  
  apa.url = paste("http://www.ots.at/api/liste?app=", AppID,
                  "&query=","",   
                  "&channel=", channel,         # Finanzen, ...
                  "&inhalt=", content,          # filter for pdfs, images, ...
                  "&von=", from,                # unix time stamp
                  "&bis=", to,                  # unix time stamp
                  "&anz=", number,              # mind. , standard , max.
                  "&felder=", "",               # CHECK if that works! use %20 as space? see: http://api.ots.at/doku/
                  "&format=", format,           # json, rss
                  sep = "")
  
  return(apa.url)
}
