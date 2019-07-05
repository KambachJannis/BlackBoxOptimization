###########################################################
##### API Client #####

# Config Data
base="optim.uni-muenster.de:5000/"
token="77aa1a29dc734cd0ad7ef71503b860ad"

# Functionality
if (!require("httr")) library(httr)
if (!require("jsonlite")) library(jsonlite)
apirequest = function(input, func, endpoint){
   #if(endpoint=="api"){
   #  return("Don't call the real api, you're wasting our budget! :D")
   #}
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
# libraries
library(plotly)
library(dplyr)

library(mlr)

###########################################################
# Helper function for batch requests
batch_apirequest = function(input, func, endpoint, call_counter=0){
  results = c()
  for(i in 1:ceiling(nrow(input)/50)){
    results = c(results,apirequest(input[(1+(i-1)*50):min(nrow(input),i*50),], func, endpoint))
  }
  call_counter = call_counter + nrow(input)
  return(list(results,call_counter))
}

testf = function() {
  call_counter <<- call_counter + 50
  print(call_counter)}

# Helper functions for plotting
fplot = function(sampledata,f,type3d="markers"){
  # Plot function, assuming that values are stored in column f
  if(!"z" %in% colnames(sampledata)){
  plot_ly(sampledata, intensity = as.formula(paste("~",f,sep="")),
          colors = colorRamp(c("blue","green", "red"))) %>%
    add_trace(x = ~x, y = ~y, z = as.formula(paste("~",f,sep="")), type = 'mesh3d') %>%
    layout(title=paste("Function Landscape Approximation for",f),
           scene = list(xaxis = list(title="X"),
                        yaxis = list(title="Y"),
                        zaxis = list(title="Function Value")))
  } else{
    if(type3d=="markers"){
      plot = plot_ly(sampledata,type="scatter3d", mode="markers",marker = list(color = as.formula(paste("~",f,sep="")),colorscale = "Viridis", reversescale=T, opacity=0.7, showscale = TRUE))
    }
    else{plot = plot_ly(sampledata,type="isosurface",colorscale = "Viridis", reversescale=T, value=as.formula(paste("~",f,sep="")))}
    
    plot = plot %>% layout(title=paste("Function Landscape Approximation for",f),
                           scene = list(xaxis = list(title="X"),
                                        yaxis = list(title="Y"),
                                        zaxis = list(title="Z")))
    plot %>% add_trace(x = ~x, y = ~y, z = ~z)
  }
}

f1f2plot = function(sampledata,f_1="f1",f_2="f2",scaleit=T){
  if(scaleit){
    sampledata = sampledata %>%
    mutate_at(.vars = setdiff(colnames(sampledata),c("x","y","z")),.funs = function(x){x/max(x)})}
  plot_ly(sampledata) %>%
    add_trace(x = ~x, y = ~y, z = as.formula(paste("~",f_1,sep="")), type = 'mesh3d',name=f_1) %>%
    add_trace(x = ~x, y = ~y, z = as.formula(paste("~",f_2,sep="")), type = 'mesh3d',name=f_2) %>%
    layout(title="Function Landscape Approximation",
           scene = list(xaxis = list(title="X"),
                        yaxis = list(title="Y"),
                        zaxis = list(title="Function Value")))
}

toCol = function(x,resolution=50){
  color=(resolution*(x-min(x))/(max(x)-min(x))+1)
  return(viridisLite::viridis(resolution+1)[floor(color)])
}

quiet = function(x){
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

