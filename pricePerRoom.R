library(caret)
library(RJSONIO)
library(gdata)
library(dplyr)
library(tibble)


packages <- c("jsonlite", "dplyr", "purrr", "tibble")

purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

parseJSON <- function(filename){
  data<-fromJSON(filename)
  vars <- setdiff(names(data), c("photos", "features"))
  traindata <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)
  return(data)
} 

pprData<-traindata[c("bathrooms", "bedrooms", "listing_id", "price", "interest_level")]

pprDataNum<-lapply(pprData[c("bedrooms", "bathrooms", "price")], as.numeric)



ppr <- function(data){
  pricePerRoom <- pprDataNum$price / (pprDataNum$bedrooms + pprDataNum$bathrooms)
  
}

#aptDataCols<-names(aptData)

# grabJ<-function(var){
#   print(paste("Variable", var, sep=""))
#   sapply(aptData, function(x) returnData(x, var))
# }
# 
# returnData<-function(x,var){
#   if(!is.null(x[[var]])){
#     return(trim(x[[var]]))
#   }else{
#     return(NA)
#   }
# }
# 
# fmDataDF<-data.frame(sapply(1:22, grabJ), stringsAsFactors=FALSE)

for 

require(reshape2)
aptData$id <- rownames(aptData) 
melt(aptData)
