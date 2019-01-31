library(foreign)
library(nnet)
library(stargazer)


image_1 <- c('d1_score'=0.1,'d2_score'=0.2,'d3_score'=0.1,'d4_score'=0.2,'d5_score'=0.4,'location'="East_Anglia",'weather'="wet",'crop_variety'="wheat_breed_1",'soil_type'="clay",'disease'="d1")
data<- as.data.frame(image_1)
data<-t(data)
data<-as.data.frame(data)

dis_data2<-data
for(i in 1:10){
  dis_data2<- rbind(dis_data2,c(0.5+0.01*i,0.3-0.01*i,0.2,0,0,"East_Anglia","wet","wheat_breed_1","clay","d1"))
}

for(i in 1:10){
dis_data2<- rbind(dis_data2,c(0.3-0.01*i,0.5+0.01*i,0.2,0,0,"East_Anglia","dry","wheat_breed_1","sandy","d2"))
}

for(i in 1:10){
  dis_data2<- rbind(dis_data2,c(0.3-0.01*i,0.2,0.5+0.01*i,0,0,"Midlands","wet","wheat_breed_1","clay","d3"))
}

# disease_data <- data
multi <- multinom(disease ~ d1_score + d2_score + d3_score + location + weather + soil_type, data=dis_data)
summary(multi)
stargazer(multi, type="text", out="multi1.htm")
multi.rrr <- exp(coef(multi))
multi.rrr
i=0
dis<- c('d1_score'=0.1,'d2_score'=0.2,'d3_score'=0.1,'location'="East_Anglia",'weather'="wet",'soil_type'="clay")
dis<-t(dis)
dis<-as.data.frame(dis)
predict(multi,dis)



# 
# disease_model.lm<-lm(disease~disease_image_score+location+weather+date+crop_variety+soil_type, # + interactions?? +treatments+irrigation
#                                   data=disease_data)
# summary(disease_model.lm)


# AIC: . Models with delta_AIC < 2 have substantial evidence, 2 < delta_AIC < 7 have considerably
# less support, and delta_AIC > 10 means that a model has essentially no support
# library(MuMIn)
# options(na.action = "na.fail") # we need to set this option for dredge to work
# disease.dredge <- dredge(disease_model.lm)
# disease.dredge
# 
# # importance: As a rule of thumb, a variable with score >0.8 is highly important, between 0.8 and 0.5 is moderately important, and <0.5 is not important
# importance(disease.dredge)