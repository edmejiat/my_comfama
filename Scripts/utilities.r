
########################################################################################
# 
#  UTILITIES
# 
########################################################################################


#---------------------------------------------------------------------------------------
# osPathJoin 
#---------------------------------------------------------------------------------------
osPathJoin <- function(...){
  normalizePath(file.path(...), winslash = "/", mustWork = F)
}

#---------------------------------------------------------------------------------------
# Workspace (paths) definition
#---------------------------------------------------------------------------------------
initialize <- function(cascadeoPath){
  
  dir.create(osPathJoin(cascadeoPath, "Datos"))
  dir.create(osPathJoin(cascadeoPath, "Diccionarios"))
  dir.create(osPathJoin(cascadeoPath, "Documentos"))
  dir.create(osPathJoin(cascadeoPath, "Resultados"))
  dir.create(osPathJoin(cascadeoPath, "Scripts"))
}


setWorkspace <- function(cascadeoPath){
  
  datosPath        <<- osPathJoin(cascadeoPath, "Datos")
  diccionarioPath           <<- osPathJoin(cascadeoPath, "Diccionarios")
  documentosPath         <<- osPathJoin(cascadeoPath, "Documentos")
  resultadosPath      <<- osPathJoin(cascadeoPath, "Resultados")
  scriptsPath    <<- osPathJoin(cascadeoPath, "Scripts")
  
  
}

#---------------------------------------------------------------------------------------
# Workspace (paths) definition
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
# Library load
#---------------------------------------------------------------------------------------

suppressWarnings(loadLibraries <- function(){
  
  library(bit64)
  library(data.table)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(plyr)
  library(dplyr)  
  library(readxl)
  library(tictoc)
  library(ggplot2)
  library(leaflet)
  library(tools)
  library(segmented)
  library(rootSolve)
  library(xlsx) 
  library(svMisc)
  library(readxl) 
  
})

# ===================================================================
# Utilities Functions
# ===================================================================

# Definition of useful functions to use accross the other scripts

#---------------------------------------------------------------------------------------
# getNextPeriod
#---------------------------------------------------------------------------------------
next_period<-function(x){
  
  x%>%as.character()
  if (substr(x,5,6)%>%as.integer() <12) {
    x=x%>%as.integer() +1
  }else{
    x=paste((substr(x,1,4)%>%as.integer()+1)%>%as.character(),"01",sep = "")%>%as.integer()
  }
  return(x)
}

#---------------------------------------------------------------------------------------
# imputeFunction
#---------------------------------------------------------------------------------------
imputeFunction <- function(x, imputeValue){
  if(!is.na(imputeValue)){
    x[is.na(x)] <- imputeValue
  }
  return(x)
}

#---------------------------------------------------------------------------------------
# convertXLSdate
#---------------------------------------------------------------------------------------
convertXLSdate <- function(x) {
  as.POSIXct(as.numeric(x) * 86400, origin="1899-12-30", tz="GMT") # (60*60*24)
}

#---------------------------------------------------------------------------------------
# colsToNum
#---------------------------------------------------------------------------------------
colsToNum <- function(df, colToStart){
  for (var in names(df)[colToStart:ncol(df)]){
    set(df, j = var, value = coerceNumeric(df[[var]], ","))
  }
  return(df)
}

#---------------------------------------------------------------------------------------
# colsNaTo0 
#---------------------------------------------------------------------------------------
colsNaTo0 <- function(df, colToStart){
  for (var in names(df)[colToStart:ncol(df)]){
    set(df, j = var, value = imputeFunction(df[[var]], 0))
  }
  return(df)
}

#---------------------------------------------------------------------------------------
# Set col first
#---------------------------------------------------------------------------------------
setcolsfirst <- function(dataTable, firstCols) {
  
  originalOrder <- copy(names(dataTable))
  
  # Error control
  errors <- firstCols[!(firstCols %in% originalOrder)]
  
  if (length(errors) != 0) {
    message <- paste0("The following variables are not in the dataset : ", errors)
    print(message)
    stop()
    
  } else {
    
    newOrder <- originalOrder[!(originalOrder %in% firstCols)]
    newOrder <- c(firstCols, newOrder)
    
    setcolorder(dataTable, newOrder)  
  }
}


#---------------------------------------------------------------------------------------
# Outersect
#---------------------------------------------------------------------------------------
outersect <- function(x,y){
  l <- unique(c(setdiff(x,y), setdiff(y,x)))
  return(l)
}

#---------------------------------------------------------------------------------------
# coerceNumeric
#---------------------------------------------------------------------------------------
coerceNumeric <- function(x, format){
  switch(
    format,
    "," = {
      x <- str_replace(x, ",", ".") %>% as.numeric()
    },
    "." = {
      x <- as.numeric(x)
    },
    ";" = {
      x <- coerceNumeric(str_replace(x, "\\.", ""), ",")
    },
    ",." = {
      x <- str_replace(x, ",", "") %>% as.numeric()
    },
    stop("The format must be one of ',','.',';' or ',.'")
  )
  return(x)
}

#---------------------------------------------------------------------------------------
# mapBinary (mapeo a variable binaria)
#---------------------------------------------------------------------------------------
mapBinary <- function(x, format, delim = "/"){
  
  if(stringr::str_length(format) > 0){
    options <- stringr::str_split(format, delim) %>% unlist
    x <- plyr::mapvalues(x, options, c(0, 1))
  }else{
    warning("No binary format defined")
  }
  
  return(x)
}

#---------------------------------------------------------------------------------------
# coerceDate
#---------------------------------------------------------------------------------------
coerceDate <- function(x, format){
  if(format == "dmy_hms"){
    x <- dmy_hms(x)
  } else {
    if(format == "ymd_hms"){
      
      x <- ymd_hms(x)
    } else {
      # x <- parse_date_time(x, format) %>% ymd
      x <- parse_date_time(x, format) %>% date
    }
  }
  return(x)
}

#---------------------------------------------------------------------------------------
# Leer todas las hojas de un excel
#---------------------------------------------------------------------------------------
read_excel_allsheets <- function(filename, tibble = FALSE) {
  
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

#---------------------------------------------------------------------------------------
# quitar acentos
#---------------------------------------------------------------------------------------


rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}
#---------------------------------------------------------------------------------------
# all source
#---------------------------------------------------------------------------------------

# source("Scripts/prepare_planta.R")
# source("Scripts/all_retos.R")
# source("Scripts/Equipos/madrinas_reto.R")
# source("Scripts/Equipos/ejec_preferenciales_reto.R")
# source("Scripts/Equipos/ejec_pyme_reto.R")
# source("Scripts/Equipos/asesor_preferenciales_reto.R")
# source("Scripts/Equipos/asesor_moviles_reto.R")
# source("Scripts/Equipos/gerente_gobierno_reto.R")
# source("Scripts/Equipos/leasing_reto.R")
# source("Scripts/Equipos/me_reto.R")
# source("Scripts/Equipos/factoring_reto.R")




