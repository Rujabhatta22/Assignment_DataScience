library(tidyverse)

getwd()
setwd("E:/")
setwd("/Data")



#liner regression 
Towns = read_csv("Towns.csv")%>% 
  select(shortPostcode, Town, District, County)
prices = read_csv("housePricesclean.csv") %>% 
  na.omit()

speeds = read_csv("cleanBroadBandspeeds.csv") %>% 
  na.omit()  

crime=read_csv("cleanCrime.csv") %>% 
  rename("DrugCount"="n") %>% 
  na.omit()



schools=read_csv("CleanSchool.csv") %>% 
  na.omit()




#------------------------------House prices vs Download Speed----------------------------------------


options(scipen=999)

HousePrices = prices %>%
  filter(Year=="2020") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))

BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  summarise(AverageDownload=mean(AverageDownload))

lm_res = HousePrices %>% left_join(BroardbandSpeeds,by="Town")
model = lm(data= lm_res, Price~AverageDownload)
summary(model)

color= c("MERSEYSIDE" = "red", "GREATER MANCHESTER" = "blue")

ggplot(lm_res,aes(x=AverageDownload,y=Price)) +
  geom_point(data = filter(lm_res,County.x=="GREATER MANCHESTER"),aes(color="Greater Mancehster"))+
  geom_point(data = filter(lm_res,County.x=="MERSEYSIDE"), aes(color="Merseyside")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Download Speed (Mbit/s)",y="Price (£)",title="House Prices vs Download Speed",color="County")



#-----------------------------------------------------------------------------------------

#----------------------------------House price and drug offence--------------------------------------------------


HousePrices = prices %>%
  filter(Year=="2020") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))


Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  filter(CrimeType=="Drugs") %>% 
  tally() %>% 
  na.omit()

colnames(Drugs)=c("Town","County","DrugCount")

lm_res6 = HousePrices %>% left_join(Drugs ,by="Town") %>%
  group_by(Town,County.x,Price) %>% 
  na.omit()

model1 = lm(data= lm_res6, Price~DrugCount)
summary(model1)

color= c("MERSEYSIDE" = "yellow", "GREATER MANCHESTER" = "Green")

ggplot(lm_res6,aes(x=Price,y=DrugCount)) +
  geom_point(data = filter(lm_res6,County.x=="GREATER MANCHESTER"),aes(color="Greater Mancehster"))+
  geom_point(data = filter(lm_res6,County.x=="MERSEYSIDE"), aes(color="Merseyside")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Town",y="Drug count",title="House Prices vs Drug",color="County")

#----------------------------Drug and  school -----------------------------------
#mistake
school_lm= schools %>%
  left_join(Towns,by=c("shortPostCode"="shortPostcode") )%>%  
  group_by(Town,County) %>%
  summarise(score=mean(Attainment8Score)) 
  

Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  filter(CrimeType=="Drugs") %>% 
  tally() %>% 
  na.omit()
colnames(Drugs)=c("Town","County","DrugCount")

lm_res2 = school_lm %>% left_join(Drugs ,by="Town") %>% 
  na.omit()
model2 = lm(data= lm_res2,score~DrugCount)
summary(model2)

colors1= c("MERSEYSIDE" = "yellow", "GREATER MANCHESTER" = "Green")

ggplot(lm_res2,aes(x=DrugCount,y=score)) +
  geom_point(data = filter(lm_res2,County.x=="GREATER MANCHESTER"),aes(color="Greater Mancehster"))+
  geom_point(data = filter(lm_res2,County.x=="MERSEYSIDE"), aes(color="Merseyside")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Drugs",y="score",title="school score vs Drug",color="County")


#---------------------------------Average download and drug-------------------
BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County,District) %>%
  summarise(AverageDownload=mean(AverageDownload))


Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  filter(CrimeType=="Drugs") %>% 
  na.omit()

lm_res3 = BroardbandSpeeds %>% left_join(Drugs ,by="Town") %>% 
  na.omit()
model3 = lm(data= lm_res3, AverageDownload~DrugCount)
summary(model3)

color= c("MERSEYSIDE" = "yellow", "GREATER MANCHESTER" = "Green")

ggplot(lm_res3,aes(x=DrugCount,y=AverageDownload)) +
  geom_point(data = filter(lm_res3,County.x=="GREATER MANCHESTER"),aes(color="GREATER MANCHESTER"))+
  geom_point(data = filter(lm_res3,County.x=="MERSEYSIDE"), aes(color="Merseyside")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Drug count",y="Average Download",title="AVergae download speed vs Drug",color="County")

#-----------------------average dwnload and school-------------------------

school_lm= schools %>%
  left_join(Towns,by=c("shortPostCode"="shortPostcode") )%>%  
  group_by(Town,County,District) %>%
  summarise(score=mean(Attainment8Score)) %>% 
  na.omit()


lm_res4 = BroardbandSpeeds %>% left_join(school_lm,by="Town.x") %>% 
  na.omit()
model4 = lm(data= lm_res4, AverageDownload~score)
summary(model4)

colors1= c("MERSEYSIDE" = "yellow", "GREATER MANCHESTER" = "Green")

ggplot(lm_res4,aes(x=score,y=AverageDownload)) +
  geom_point(data = filter(lm_res4,County=="GREATER MANCHESTER"),aes(color="Greater Mancehster"))+
  geom_point(data = filter(lm_res4,County=="MERSEYSIDE"), aes(color="Merseyside")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Score",y="Average Download",title="score vs AverageDownload",color="County")






