library(keras)
library(rjson)
library(magick)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(XML)
library(xml2)
library(jsonlite)
library(tensorflow)
###############################################################################################
# mock training data

# only one disease at a time
# widely, independently and randomly sampled data
# prevalence data?
# independence issues? esp with eg location and weather

# inputs:
# image data (with date and location tag?),
# date
# location (discretise?),
# weather/rainfall?,
# weather/mean temperature?,
# weather - air temperature, rainfall, relative humidity (RH), and LWD - see paper
# crop variety,
# soil type,
# treatments applied (and when),
# irrigation
###############################################################################################
# default is
# location<-'East_Anglia'
# soil<-"clay"
# WB <- 'WB1'
# mean values
# rainfall av
rain_av<-list()
rain_av[[1]]<-40
rain_av[[2]]<-50
rain_av[[3]]<-60  # wetter
# temp av
temp_av<-list()
temp_av[[1]]<-16
temp_av[[2]]<-18  # hottest
temp_av[[3]]<-14  # coldest
# weather standard dev
rainfall_sd<-10
temp_sd<-1
# location bias
loc_bias <- list()
loc_bias[[1]]  <-  0.1 # usually EA
loc_bias[[2]]  <-  0   # no preference
loc_bias[[3]]  <- -0.1 # usually Midlands
# crop variety bias
crop_bias <- list()
crop_bias[[1]] <-  0.1 # usually clay
crop_bias[[2]] <- -0.1 # usually sandy
crop_bias[[3]] <- -100 # always sandy
# soil type bias
soil_bias <- list()
soil_bias[[1]] <- -100 # always WB2
soil_bias[[2]] <- -0.1 # usually WB2
soil_bias[[3]] <- -0.1 # usually WB1
###############################################################################################
# name of d1, d2, d3 ... di
name_disease <- list()
for(i in 1:length(params$label_names)){
  name_disease[[i]]<-colnames(tr_analysis$class_predictions)[i] 
}
###############################################################################################
# image_data for d1, d2, d3 ... di
image_data_table <- list()
for(i in 1:length(params$label_names)){
  image_data_table[[i]]<-filter(tr_analysis$class_predictions,label==name_disease[[i]]) 
}
###############################################################################################
# initialise empty d frame
dis_data2<-as.data.frame(t(c('disease'=numeric(0),'d1_score'=numeric(0),'d2_score'=numeric(0),'d3_score'=numeric(0),'location'=numeric(0),'rainfall'=numeric(0),'mean_temp'=numeric(0),'crop_variety'=numeric(0),'soil_type'=numeric(0))))
# add data
for(i in 1:length(params$label_names)){
  dis_data2<-fake_data_creator(dis_data2,name_disease[[i]],image_data_table[[i]],loc_bias[[i]],crop_bias[[i]],soil_bias[[i]],rain_av[[i]],temp_av[[i]]) # add di data
}

dis_data2<-format_data(dis_data2) # gets it into the right form
head(dis_data2)
