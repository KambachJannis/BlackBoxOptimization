if (!require("httr")) library(httr)
if (!require("jsonlite")) library(jsonlite)
apirequest = function(input, func, endpoint){
  # if(endpoint=="api"){
  #   return("Access denied! :)")
  # }
  input_intermediate = 1:nrow(input)
  for(i in 1:nrow(input)){
    input_intermediate[i]=paste0(input[i,],collapse = ",")
  }

  data=paste0(input_intermediate, collapse =";")
  
  call= paste(base,"/",endpoint,"/",func,"/",token,"/",data,sep="")
  
  #API-Request
  data_raw=GET(call)
  
  #extracting content from API response, convert to JSON
  data_json=content(data_raw,"text")
  

  #parsing the JSON format
  data_json_parsed = fromJSON(data_json, flatten = TRUE)
  
  #Check if error occured
  if (names(data_json_parsed)[1]!= "data") {
    print(data_json)
    return(data_json_parsed)
  }
  #converting to a data frame
  data_df = as.data.frame(data_json_parsed)
  
  #Convert data to string
  data_string=as.character(data_df$data)
  
  #Replace '[' and ']'
  data_string=gsub("\\[","",data_string)
  data_string=gsub("\\]","",data_string)
  
  #Seperate data
  split=strsplit(data_string,split=", ")
  split=unlist(split)
  
  #convert to double
  data=as.double(split)
  return (data)
}

base="optim.uni-muenster.de:5000/"
token="YOUR TOKEN"

input = data.frame(x=c(2,0.4,9), y=c(3.4, 0.5,9))
response = apirequest(input, 2, "api-test2D")
response