###########################################################
##### API Client #####

# Config Data
base="optim.uni-muenster.de:5000/"
token="77aa1a29dc734cd0ad7ef71503b860ad"

# Functionality
if (!require("httr")) library(httr)
if (!require("jsonlite")) library(jsonlite)
apirequest = function(input, func, endpoint){
   if(endpoint=="api"){
     return("Don't call the real api, you're wasting our budget! :D")
   }
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

###########################################################
# Helper function for batch requests
batch_apirequest = function(input, func, endpoint){
  results = c()
  for(i in 1:ceiling(nrow(input)/50)){
    results = c(results,apirequest(input[(1+(i-1)*50):min(nrow(input),i*50),], func, endpoint))
  }
  return(results)
}

###########################################################
##### Optimization #####
# Test calls

samples = as.data.frame(expand.grid(seq(0,20,by=1),seq(0,20,by=1)))
colnames(samples)=c("x","y")
response = batch_apirequest(samples, 2, "api-test2D")
samples$f = response #the function value

# Plotting example (3D plot)
library(plotly)

plot_ly(samples, intensity = ~f,
        colors = colorRamp(c("blue","green", "red"))) %>%
  add_trace(x = ~x, y = ~y, z = ~f, type = 'mesh3d') %>%
  layout(title="Function Landscape Approximation",
         scene = list(xaxis = list(title="X"),
                      yaxis = list(title="Y"),
                      zaxis = list(title="Function Value")))

