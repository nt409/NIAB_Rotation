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


# default is
# location<-'East_Anglia'
# soil<-"clay"
# WB <- 'WB1'
# mean values
# rainfall av
r1_av<-40
r2_av<-50
r3_av<-60  # wetter
# temp av
t1_av<-16
t2_av<-18  # hottest
t3_av<-14  # coldest
# weather standard dev
rainfall_sd<-10
temp_sd<-1
# start numbers
start_no_1  <- 2
start_no_2  <- 1
start_no_3  <- 1
# location bias
loc_bias_1  <-  0.1 # usually EA
loc_bias_2  <-  0   # no preference
loc_bias_3  <- -0.1 # usually Midlands
# crop variety bias
crop_bias_1 <-  0.1 # usually clay
crop_bias_2 <- -0.1 # usually sandy
crop_bias_3 <- -100 # always sandy
# soil type bias
soil_bias_1 <- -100 # always WB2
soil_bias_2 <- -0.1 # usually WB2
soil_bias_3 <- -0.1 # usually WB1

name_1<-colnames(tr_analysis$class_predictions)[1]
name_2<-colnames(tr_analysis$class_predictions)[2]
name_3<-colnames(tr_analysis$class_predictions)[3]

image_data_1 <- filter(tr_analysis$class_predictions,label==name_1)
image_data_2 <- filter(tr_analysis$class_predictions,label==name_2)
image_data_3 <- filter(tr_analysis$class_predictions,label==name_3)

# first data entry
data<- as.data.frame(t(c('disease'=name_1,'d1_score'=image_data_1[1,1],'d2_score'=image_data_1[1,2],'d3_score'=image_data_1[1,3],'location'="East_Anglia",'rainfall'=50,'mean_temp'=16,'crop_variety'="WB1",'soil_type'="clay")))
#data<- as.data.frame(t(c(disease=numeric(0),d1_score=numeric(0),d2_score=numeric(0),d3_score=numeric(0),location=numeric(0),rainfall=numeric(0),mean_temp=numeric(0),crop_variety=numeric(0),soil_type=numeric(0))))
dis_data2<-data

#dis_data2<-fake_data_creator(name_1,image_data_1,start_number,loc_bias,crop_bias,soil_bias)
dis_data2<-fake_data_creator(dis_data2,name_1,image_data_1,start_no_1,loc_bias_1,crop_bias_1,soil_bias_1,r1_av,t1_av) # add d1
dis_data2<-fake_data_creator(dis_data2,name_2,image_data_2,start_no_2,loc_bias_2,crop_bias_2,soil_bias_2,r2_av,t2_av) # add d2
dis_data2<-fake_data_creator(dis_data2,name_3,image_data_3,start_no_3,loc_bias_3,crop_bias_3,soil_bias_3,r3_av,t3_av) # add d3


dis_data2<-format_data(dis_data2) # gets it into the right form
head(dis_data2)
