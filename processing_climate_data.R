## paquetes requeridos

#install.packages("ncdf4")
#install.packages("raster")
#install.packages("rgdal")
#install.packages("tidyverse")
#install.packages("R.utils")
#install.packages("ggplot2")
#install.packages("ggrepel")
#install.packages("readxl")
#install.packages("purrr")
#install.packages("tidyverse")
#install.packages("sf")
#install.packages("sp")
#install.packages("rgdal")

##### Instalar paquetes.

library(ncdf4)
library(raster)
library(rgdal)
library(tidyverse)
library(R.utils)
library(ggplot2)
library(ggrepel)
library(readxl)
library(purrr)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(collapse)


###
### El análisis se hizo en una carpeta llamada  raw_climate_data, que coniene, a su vez, dos carpetas: 
### precip_2017
### temp_2017
## estas últimas dos vacías. 

setwd("..\\raw_climate_data")


# Download the precipitation data data

download.file("http://climate.geog.udel.edu/~climate/html_pages/Global2017/precip_2017.tar.gz", "precip_2017.tar.gz" )
gunzip("precip_2017.tar.gz")
untar("precip_2017.tar", exdir = "precip_2017")

# Download the temperature data

download.file("http://climate.geog.udel.edu/~climate/html_pages/Global2017/air_temp_2017.tar.gz", "temp_2017.tar.gz" )
gunzip("temp_2017.tar.gz")
untar("temp_2017.tar", exdir = "temp_2017")


############################################

WORD <- st_read("TM_WORLD_BORDERS-0.3.shp") 

setwd("temp_2017")

ldf <- list() # creates a list
list <- dir(pattern = "air_temp.*") # creates the list of all the csv files in the directory


###

for (k in 1:length(list)){
  
  ldf[[k]] <- read.table(list[k], quote="\"", comment.char="")
  
}

sf::sf_use_s2(FALSE)  ### apaga un error 

ldf2 <- list() # creates a list


for (k in 1:length(list)){
  
  names(ldf[[k]])[names(ldf[[k]]) == "V1"] <- "lon"
  names(ldf[[k]])[names(ldf[[k]]) == "V2"] <- "lat"
  
  ldf[[k]]$temp<- rowSums(ldf[[k]][,3:15], na.rm=TRUE)/12
  
  ldf[[k]] =  ldf[[k]][ -c(3:15) ]  ### eliminando meses 
  
  ldf[[k]]<- st_as_sf(ldf[[k]], coords=c("lon","lat"), crs=4326, remove=FALSE) 
  
  ldf[[k]] <- ldf[[k]] %>% st_transform(st_crs(WORD))
  
  
  ldf2[[k]] <- st_join(x = ldf[[k]], y = WORD)
  
}


Panel_temp= as.data.frame(ldf2[[1]])
Panel_temp= collap(Panel_temp, temp ~ NAME +ISO2+ISO3, FUN = list(fmean)) 
Panel_temp$ano <-  c(rep(1900, nrow(Panel_temp)))


for (k in 2:length(list)){
  
  data_flag= as.data.frame(ldf2[[k]])
  data_flag= collap(data_flag, temp ~ NAME +ISO2+ISO3, FUN = list(fmean))
  a= k+1899
  data_flag$ano <-  c(rep( a, nrow(data_flag)))
  
  Panel_temp= rbind(Panel_temp, data_flag)
  
  
}

rm(data_flag)


######## with precipitations: 

setwd("..\\precip_2017")


ldf <- list() # creates a list
list <- dir(pattern = "precip.*") # creates the list of all the files in the directory


for (k in 1:length(list)){
  
  ldf[[k]] <- read.table(list[k], quote="\"", comment.char="")
  
  
}


sf::sf_use_s2(FALSE)  ### apaga un error 

ldf3 <- list() # creates a list

for (k in 1:length(list)){
  
  names(ldf[[k]])[names(ldf[[k]]) == "V1"] <- "lon"
  names(ldf[[k]])[names(ldf[[k]]) == "V2"] <- "lat"
  
  ldf[[k]]$precip<- rowSums(ldf[[k]][,3:15], na.rm=TRUE)/12
  
  ldf[[k]] =  ldf[[k]][ -c(3:15) ]  ### eliminando meses 
  
  ldf[[k]]<- st_as_sf(ldf[[k]], coords=c("lon","lat"), crs=4326, remove=FALSE) 
  
  ldf[[k]] <- ldf[[k]] %>% st_transform(st_crs(WORD))
  
  
  ldf3[[k]] <- st_join(x = ldf[[k]], y = WORD)
  
  
  
}


Panel_prep= as.data.frame(ldf3[[1]])
Panel_prep= collap(Panel_prep, precip ~ NAME +ISO2+ISO3, FUN = list(fmean)) 
Panel_prep$ano <-  c(rep(1900, nrow(Panel_prep)))


for (k in 2:length(list)){
  
  data_flag= as.data.frame(ldf3[[k]])
  data_flag= collap(data_flag, precip ~ NAME +ISO2+ISO3, FUN = list(fmean))
  a= k+1899
  data_flag$ano <-  c(rep( a, nrow(data_flag)))
  
  Panel_prep= rbind(Panel_prep, data_flag)
  
  
}

rm(data_flag)


#################################################################### EXPORTAR LA BASE DE DATOS

global_climate_data= merge(Panel_prep, Panel_temp, by=c("NAME","ISO2","ISO3", "ano"), all=TRUE)

library(openxlsx)

setwd("C:\\Users\\Andres Felipe\\Desktop\\Proyectos\\Datos Clim?ticos")

write.xlsx(global_climate_data, "global_clim.xlsx", asTable = FALSE, overwrite = TRUE)





