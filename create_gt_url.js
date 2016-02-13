'use strict';


function URL_GT(keyword, country, region, year, month, length){
  
  var start = "http://www.google.com/trends/trendsReport?hl=en-US&q=";
  var end = "&cmpt=q&content=1&export=1";
  var geo = "";
  var date = "";
  var URL = "";
  var month=1;
  var length=3;

  
  //Geographic restrictions
  if(typeof country!=="undefined") {
    geo="&geo=";
    geo=geo + country;
    if(region!==undefined) geo=geo + "-" + region;
  }
  
  if(typeof keyword==="string"){
  	var queries=keyword;
  }
  
  if(typeof keyword==="object"){
  	var queries=keyword[0];
    for(var i=1; i < keyword.length; i++){
      queries=queries + "%2C" + keyword[i];
    }
  }
  
  //Dates
  if(typeof year!=="undefined"){
    date="&date="
    date=date + month + "%2F" + year + "%20" + length + "m"
  }
  
  URL = start + queries + geo + date + end;
  URL = URL.replace(" ", "%20");
  return(URL);
}
