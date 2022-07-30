library(tidyverse)


getwd()
setwd("E:/")
setwd("/Data")



#---------------house rank------
Towns = read_csv("Towns.csv")%>% 
  select(shortPostcode, Town, District, County)

duplicated(Towns$District)
Towns=Towns [!duplicated(Towns$District),]
  

House_price = read_csv("housePricesclean.csv") %>% 
  na.omit()

#house rank
Houseprice= House_price %>%
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()
housePrices=Houseprice  %>% 
  filter(Year=="2020") %>% 
  group_by(District) %>% 
  summarise(Price=mean(Price)) %>% 
  arrange(Price) %>% 
  mutate(HouseScore=10-(Price/120000)) %>% 
  select(District,HouseScore)
housePrices


#download rank

speed_downloads = read_csv("cleanBroadBandspeeds.csv") %>% 
  na.omit()

Speed_Download = speed_downloads %>%
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()
colnames(speed_download)=c("1","ID","shortPostcode","AverageDownload","MaxDownload","Averageupload","Maxupload")

download_speed=Speed_Download%>% 
  group_by(District) %>% 
  summarise(downloadSpeed=AverageDownload) %>% 
  arrange(-downloadSpeed) %>% 
  mutate(DownloadScore=10-(downloadSpeed/1200)) %>% 
  select(District,DownloadScore)
 


download_speed



#crime score rank
crime_score=read_csv("cleanCrimes.csv") %>% 
  rename("CrimeCount"="n")
crime_rank = crime_score %>%
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()


crime_rank=crime_rank%>% 
  group_by(District) %>% 
  summarise(score=mean(CrimeCount)) %>% 
  arrange(desc(score)) %>% 
  mutate(score=10-(score/1200)) %>% 
  rename("crimescore"="score") %>% 
select(District,crimescore) 
crime_rank


#school score
school_score=read_csv("cleanSchool.csv")
school_rank = school_score %>%
  rename(shortPostcode=shortPostCode) %>% 
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()

school_rank=school_rank%>% 
  group_by(SchoolName,District) %>% 
  summarise(score=mean(Attainment8Score)) %>% 
  arrange(score) %>% 
  mutate(score=10-(score/100)) %>% 
  select(SchoolName,score,District)
school_rank




#overall rank
# OVERALL RANKING



RankingMerge = housePrices%>% 
  left_join(download_speed, by = "District") %>% 
  left_join(crime_rank, by = "District") %>% 
  left_join(school_rank, by = "District")


RankingMerge$SchoolName[is.na(RankingMerge$SchoolName)] <- "Not available"
RankingMerge$SchoolScore[is.na(RankingMerge$score)] <- 0

overallRank = RankingMerge %>% 
  group_by(HouseScore, score,DownloadScore,crimescore) %>%
  mutate(overallScore = (HouseScore + score + DownloadScore +crimescore)/4) %>% 
  arrange(-overallScore)

overallRank
