
# only one disease at a time
# independently and randomly sampled data
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

# mean values
d1_av1<-0.9
d1_av2<-0.05
d1_av3<-0.05

d2_av1<-0.05
d2_av2<-0.9
d2_av3<-0.05

d3_av1<-0.05
d3_av2<-0.05
d3_av3<-0.9
#rainfall av
r1_av<-40
r2_av<-50
r3_av<-60
#temp av
t1_av<-16
t2_av<-18
t3_av<-14

rainfall_sd<-10
temp_sd<-1
d_sd_1<-0.2
d_sd_2<-0.2
d_sd_3<-0.2

d_var2<-0.05

## d1 - clay soil only, preference for wet, Midlands , Wheat Breed 2
q<-rnorm(1,mean=0,sd=1)
data<- as.data.frame(t(c('disease'="d1",'d1_score'=d1_av1+d_var2*q,'d2_score'=d1_av2-d_var2*q,'d3_score'=d1_av3,'location'="East_Anglia",'rainfall'=50,'mean_temp'=16,'crop_variety'="WB1",'soil_type'="clay")))
dis_data2<-data

for(i in 2:200){
  #initialise random variables
  q<-rnorm(1,mean=0,sd=1)
  q2<-rnorm(1,mean=0,sd=1)
  w<-rnorm(1,mean=0,sd=1)
  w2<-rnorm(1,mean=0,sd=1)
  l<-rnorm(1,mean=0,sd=1)
  s<-rnorm(1,mean=0,sd=1)
  t<-rnorm(1,mean=0,sd=1)
  # data
  location<-'East_Anglia'
  soil<-"clay"
  WB <- 'WB1'
  rainfall <- r1_av+rainfall_sd*w
  mean_temp<-t1_av+temp_sd*w2
  d1_score<-d1_av1+d_sd_1*q
  d2_score<-d1_av2-d_sd_1*q
  d3_score<-d1_av3
  # adjustments
  if(l>-0.1){location='Midlands'}
  if(t>-0.1){WB='WB2'}
  if(q>0.1){d1_score<-0.05
  d2_score<-0.7+d_var2*q2
  d3_score<-0.5-d_var2*q2} # image classifier gets it completely wrong in this case
  # add into data frame
  dis_data2<-rbind(dis_data2,t(c('disease'="d1",'d1_score'=d1_score,'d2_score'=d2_score,'d3_score'=d3_score,'location'=location,'rainfall'=rainfall,'mean_temp'=mean_temp,'crop_variety'=WB,'soil_type'=soil)))
}

## d2 - occurs in both locations, both weathers, both soil types but with preferences
## d2 preference for sandy, midlands, wet, WB2
for(i in 1:200){
  #initialise random variables
  q<-rnorm(1,mean=0,sd=1)
  q2<-rnorm(1,mean=0,sd=1)
  w<-rnorm(1,mean=0,sd=1)
  w2<-rnorm(1,mean=0,sd=1)
  l<-rnorm(1,mean=0,sd=1)
  s<-rnorm(1,mean=0,sd=1)
  t<-rnorm(1,mean=0,sd=1)
  # data
  location<-'East_Anglia'
  soil<-'clay'
  WB <- 'WB1'
  rainfall <- r2_av+rainfall_sd*w
  mean_temp<-t2_av+temp_sd*w2
  d1_score<-d2_av1-d_sd_2*q
  d2_score<-d2_av2+d_sd_2*q
  d3_score<-d2_av3
  # adjustments
  if(l>-0.5){location='Midlands'}
  if(s>-0.4){soil='sandy'}
  if(t>-0.5){WB='WB2'}
  if(q>0.05){d1_score<-0.25+d_var2*q2
  d2_score<-0.05
  d3_score<-0.7-d_var2*q2} # image classifier gets it completely wrong in this case
  # add into data frame
  dis_data2<-rbind(dis_data2,t(c('disease'="d2",'d1_score'=d1_score,'d2_score'=d2_score,'d3_score'=d3_score,'location'=location,'rainfall'=rainfall,'mean_temp'=mean_temp,'crop_variety'=WB,'soil_type'=soil)))
}

## d3 - occurs in both locations, both soil types but with preferences
# d3 preference for dry, EA, clay, WB1
for(i in 1:200){
  #initialise random variables
  q<-rnorm(1,mean=0,sd=1)
  q2<-rnorm(1,mean=0,sd=1)
  w<-rnorm(1,mean=0,sd=1)
  w2<-rnorm(1,mean=0,sd=1)
  l<-rnorm(1,mean=0,sd=1)
  s<-rnorm(1,mean=0,sd=1)
  t<-rnorm(1,mean=0,sd=1)
  # data
  location<-'East_Anglia'
  soil<-'clay'
  WB <- 'WB1'
  rainfall <- r3_av+rainfall_sd*w
  mean_temp<-t3_av+temp_sd*w2
  d1_score<-d3_av1-d_sd_3*q
  d2_score<-d3_av2
  d3_score<-d3_av3+d_sd_3*q
  # adjustments
  if(l>0.3){location='Midlands'}
  if(s>0.5){soil='sandy'}
  if(t>0.5){WB='WB2'}
  if(q>0.05){d1_score<-0.8+d_var2*q2
  d2_score<-0.15-d_var2*q2
  d3_score<-0.05} # image classifier gets it completely wrong in this case
  # add into data frame
  dis_data2<-rbind(dis_data2,t(c('disease'="d3",'d1_score'=d1_score,'d2_score'=d2_score,'d3_score'=d3_score,'location'=location,'rainfall'=rainfall,'mean_temp'=mean_temp,'crop_variety'=WB,'soil_type'=soil)))
}
dis_data2

dis_data2$d1_score <- as.numeric(as.character(dis_data2$d1_score))
dis_data2$d2_score <- as.numeric(as.character(dis_data2$d2_score))
dis_data2$d3_score <- as.numeric(as.character(dis_data2$d3_score))
dis_data2$rainfall <- as.numeric(as.character(dis_data2$rainfall))
dis_data2$mean_temp <- as.numeric(as.character(dis_data2$mean_temp))

# make categorical data into numeric data taking values 0 or 1
dis_data2 <- mutate(dis_data2,
                    Loc_EA_indic = ifelse(location=="East_Anglia",1,0),
                    Loc_Midlands_indic = ifelse(location=="Midlands",1,0),
                    WB_1_indic = ifelse(crop_variety=="WB1",1,0),
                    WB_2_indic = ifelse(crop_variety=="WB2",1,0),
                    ST_clay_indic = ifelse(soil_type=="clay",1,0),
                    ST_sandy_indic = ifelse(soil_type=="sandy",1,0)
                    )

head(dis_data2)
