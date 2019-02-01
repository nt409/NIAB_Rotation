library(foreign)
library(nnet)
library(stargazer)
# library(MNP)
library(dplyr)


## d1 - clay soil only, preference for wet, Midlands
q<-rnorm(1,mean=0,sd=0.1)
data<- as.data.frame(t(c('d1_score'=0.5+q,'d2_score'=0.2-q,'d3_score'=0.1,'d4_score'=0.1,'d5_score'=0.1,'location'="East_Anglia",'weather'="wet",'crop_variety'="wheat_breed_1",'soil_type'="clay",'disease'="d1")))
dis_data2<-data

for(i in 1:200){
  q<-rnorm(1,mean=0,sd=0.1)
  w<-rnorm(1,mean=0,sd=1)
  l<-rnorm(1,mean=0,sd=1)
  s<-rnorm(1,mean=0,sd=1)
  weather<-'dry'
  location<-'East_Anglia'
  if(w>-0.3){weather='wet'}
  if(l>-0.1){location='Midlands'}
  dis_data2<-rbind(dis_data2,t(c('d1_score'=0.5+q,'d2_score'=0.3-q,'d3_score'=0.2,'d4_score'=0.1,'d5_score'=0.1,'location'=location,'weather'=weather,'crop_variety'="wheat_breed_1",'soil_type'="clay",'disease'="d1")))
}

## d2 - occurs in both locations, both weathers, both soil types but with preferences
## d2 preference for sandy, midlands, wet
for(i in 1:200){
  q<-rnorm(1,mean=0,sd=0.1)
  w<-rnorm(1,mean=0,sd=1)
  l<-rnorm(1,mean=0,sd=1)
  s<-rnorm(1,mean=0,sd=1)
  weather<-'dry'
  location<-'East_Anglia'
  soil<-'clay'
  if(w>-0.1){weather='wet'}
  if(l>-0.5){location='Midlands'}
  if(s>-0.4){soil='sandy'}
  dis_data2<-rbind(dis_data2,t(c('d1_score'=0.3-q,'d2_score'=0.5+q,'d3_score'=0.2,'d4_score'=0.1,'d5_score'=0.1,'location'=location,'weather'=weather,'crop_variety'="wheat_breed_1",'soil_type'=soil,'disease'="d2")))
}

## d3 - occurs in both locations, both weathers, both soil types but with preferences
# d3 preference for dry, EA, clay
for(i in 1:200){
  q<-rnorm(1,mean=0,sd=0.1)
  w<-rnorm(1,mean=0,sd=1)
  l<-rnorm(1,mean=0,sd=1)
  s<-rnorm(1,mean=0,sd=1)
  weather<-'dry'
  location<-'East_Anglia'
  soil<-'clay'
  if(w>0.2){weather='wet'}
  if(l>0.3){location='Midlands'}
  if(s>0.5){soil='sandy'}
  dis_data2<-rbind(dis_data2,t(c('d1_score'=0.3-q,'d2_score'=0.05,'d3_score'=0.55+q,'d4_score'=0.05,'d5_score'=0.05,'location'=location,'weather'=weather,'crop_variety'="wheat_breed_1",'soil_type'=soil,'disease'="d3")))
}
dis_data2

dis_data2$d1_score <- as.numeric(as.character(dis_data2$d1_score))
dis_data2$d2_score <- as.numeric(as.character(dis_data2$d2_score))
dis_data2$d3_score <- as.numeric(as.character(dis_data2$d3_score))
dis_data2$d4_score <- as.numeric(as.character(dis_data2$d4_score))
dis_data2$d5_score <- as.numeric(as.character(dis_data2$d5_score))

# disease_data <- data
multi <- multinom(disease ~ d1_score + d2_score + d3_score + location + weather + soil_type, data=dis_data2) # but isn't treating d_score as continuous
summary(multi)
stargazer(multi, type="text", out="multi1.htm") # how many more times likely, for a unit change in one isolated variable. Also gives significance
multi.rrr <- exp(coef(multi))
multi.rrr

# predict for new data
## d1 - clay soil only, preference for wet, Midlands
## d2 preference for sandy, Midlands, wet
## d3 preference for dry, EA, clay

# works - even if looks like d1, if it's sandy will predict next best.
weather<-'wet'
soil<-'clay'
location<-'Midlands'
d1sc<-0.1
d2sc<-0.9
d3sc<-0.2

dis<- c('d1_score'=d1sc,'d2_score'=d2sc,'d3_score'=d3sc,'location'=location,'weather'=weather,'soil_type'=soil)
dis<-t(dis)
dis<-as.data.frame(dis)
dis$d1_score <- as.numeric(as.character(dis$d1_score))
dis$d2_score <- as.numeric(as.character(dis$d2_score))
dis$d3_score <- as.numeric(as.character(dis$d3_score))

Probability<-predict(multi,dis,"probs")
pred<-as.data.frame(Probability)
pred$Disease <- c(1,2,3)
frame3<-pred[order(-pred$Probability),]
frame3

# filter(dis_data2,disease=="d1")


 
# disease_model.lm<-lm(disease~disease_image_score+location+weather+date+crop_variety+soil_type, # + interactions?? +treatments+irrigation
#                                   data=disease_data)
# summary(disease_model.lm)

# AIC: . Models with delta_AIC < 2 have substantial evidence, 2 < delta_AIC < 7 have considerably
# less support, and delta_AIC > 10 means that a model has essentially no support
# library(MuMIn)
# options(na.action = "na.fail") # we need to set this option for dredge to work
# disease.dredge <- dredge(disease_model.lm)
# disease.dredge

# # importance: As a rule of thumb, a variable with score >0.8 is highly important, between 0.8 and 0.5 is moderately important, and <0.5 is not important
# importance(disease.dredge)