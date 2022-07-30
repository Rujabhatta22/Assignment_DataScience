library(tidyverse)
library(dplyr)

# Add Columns Names if they are missing 


library(tidyverse)
Towns = read_csv("E:/assignment data/cleandata/Towns.csv")
BroadbandSpeeds = read_csv("E:/assignment data/BroadbandSpeeds.csv")


BroadbandSpeeds = replace(BroadbandSpeeds,is.na(BroadbandSpeeds), 0)


cleanBroadbandSpeeds = BroadbandSpeeds %>%
  mutate(shortPostcode = str_trim(substring(postcode_space, 1,4))) %>% 
  group_by(shortPostcode) %>% 
  summarise_at(vars("Average download speed (Mbit/s)","Maximum download speed (Mbit/s)","Average upload speed (Mbit/s)",
                    "Maximum upload speed (Mbit/s)"),
               list(name = mean)) %>% 
  left_join(Towns,by="shortPostcode") %>% 
  filter(County=="GREATER MANCHESTER"|County=="MERSEYSIDE") %>% 
  arrange(County) %>% 
  select(-County,Town,District,County,Population2019,Population2020,) %>% 
  rename("AverageDownload"="Average download speed (Mbit/s)_name","MaxDownload"="Maximum download speed (Mbit/s)_name",
         "AverageUpload"="Average upload speed (Mbit/s)_name","MaxUpload"="Maximum upload speed (Mbit/s)_name") %>% 
  na.omit()

#need to write again
write.csv(cleanBroadbandSpeeds,"E:/sem4_Data_science/cleanBroadbandSpeeds.csv")




library(tidyverse)

houseprice2019=read_csv("E:/assignment data/pp-2019_2.csv")
houseprice2020=read_csv("E:/assignment data/pp-2020.csv")
houseprice2021=read_csv("E:/assignment data/pp-2021.csv")


#changed cloumn names
colnames(houseprice2019)=c("ID","Price","Date","PostCode","PAON","SAON","FL",
                           "House Num","Flat","Street name","Locality","Town",
                           "District","County","Type1","Type2")
colnames(houseprice2020)=c("ID","Price","Date","PostCode","PAON","SAON","FL",
                           "House Num","Flat","Street name","Locality","Town","
                           District","County","Type1","Type2")
colnames(houseprice2021)=c("ID","Price","Date","PostCode","PAON","SAON","FL",
                           "House Num","Flat","Street name","Locality","Town",
                           "District","County","Type1","Type2")



#data frame from price 2019 to 2020
HousePrices = houseprice2019 %>% 
  add_row(houseprice2020)%>% 
  add_row(houseprice2021)
#data clean of house price

CleaningHp<-HousePrices %>%
  filter(County =="GREATER MANCHESTER" | County=="MERSEYSIDE") %>% 
  mutate( ID = row_number()) %>% 
  na.omit()
View(CleaningHp)


#to export
write.csv(CleaningHp, "E:/sem4_Data_science/cleanData_price.csv")



cleanHousePrices = HousePrices %>%
  filter(County=="GREATER MANCHESTER"|County=="MERSEYSIDE") %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>% 
  mutate(Year=substring(Date,1,4)) %>% 
  arrange(County) %>% 
  select(PostCode,shortPostcode,Price,Year,PAON)

#exporting file
write.csv(cleanHousePrices, "E:/sem4_Data_science/HousePricesclean.csv")

PopulationData = read_csv("E:/assignment data/Population2011_1656567141570.csv")

PopulationData = PopulationData %>%  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>% 
  group_by(shortPostcode) %>%
  summarise_at(vars(Population),list(Population2011 = sum)) %>% 
  mutate(Population2012= (1.00695353132322269 * Population2011)) %>%
  mutate(Population2013= (1.00669740535540783 * Population2012)) %>%
  mutate(Population2014= (1.00736463978721671 * Population2013)) %>%
  mutate(Population2015= (1.00792367505802859 * Population2014)) %>%
  mutate(Population2016= (1.00757874492811929 * Population2015)) %>%
  mutate(Population2017= (1.00679374473924223 * Population2016)) %>%
  mutate(Population2018= (1.00605929132212552 * Population2017)) %>%
  mutate(Population2019= (1.00561255390388033 * Population2018)) %>%
  mutate(Population2020= (1.00561255390388033 * Population2019)) %>% 
  select(shortPostcode,Population2016,Population2017,Population2018,Population2019,Population2020)
 
Towns = HousePrices %>%
  filter(County=="GREATER MANCHESTER"|County=="MERSEYSIDE") %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>% 
  left_join(PopulationData,by = "shortPostcode") %>% 
  select(shortPostcode,Town,District,County,Population2016,Population2017,Population2018,
         Population2019,Population2020) %>% 
  group_by(shortPostcode) %>% 
  filter(row_number()==1) %>% 
  arrange(County) %>% 
  distinct(District) %>% 
  na.omit()
Towns=Towns [!duplicated(Towns$District),]
#export towns data
write.csv(Towns, "E:/sem4_Data_science/Towns.csv")





#crime data
cd2019006 = read_csv("E:/assignment data/crime/2019-06-greater-manchester-street.csv", show_col_types = FALSE)
cd201907 = read_csv("E:/assignment data/crime/2019-07-merseyside-street.csv", show_col_types = FALSE)
cd201906 = read_csv("E:/assignment data/crime/2019-06-merseyside-street.csv", show_col_types = FALSE)
cd201908 = read_csv("E:/assignment data/crime/2019-08-merseyside-street.csv", show_col_types = FALSE)
cd201909 = read_csv("E:/assignment data/crime/2019-09-merseyside-street.csv", show_col_types = FALSE)
cd201910 = read_csv("E:/assignment data/crime/2019-10-merseyside-street.csv", show_col_types = FALSE)
cd201911 = read_csv("E:/assignment data/crime/2019-11-merseyside-street.csv", show_col_types = FALSE)
cd201912=read_csv("E:/assignment data/crime/2019-12-merseyside-street.csv", show_col_types = FALSE)
cd202001 = read_csv("E:/assignment data/crime/2020-01-merseyside-street.csv", show_col_types = FALSE)
cd202002 = read_csv("E:/assignment data/crime/2020-02-merseyside-street.csv", show_col_types = FALSE)
cd202003 = read_csv("E:/assignment data/crime/2020-03-merseyside-street.csv", show_col_types = FALSE)
cd202004 = read_csv("E:/assignment data/crime//2020-04-merseyside-street.csv", show_col_types = FALSE)
cd202005 = read_csv("E:/assignment data/crime/2020-05-merseyside-street.csv", show_col_types = FALSE)
cd202006 = read_csv("E:/assignment data/crime/2020-06-merseyside-street.csv", show_col_types = FALSE)
cd202007 = read_csv("E:/assignment data/crime/2020-07-merseyside-street.csv", show_col_types = FALSE)
cd202008 = read_csv("E:/assignment data/crime/2020-08-merseyside-street.csv", show_col_types = FALSE)
cd202009 = read_csv("E:/assignment data/crime/2020-09-merseyside-street.csv", show_col_types = FALSE)
cd202010 = read_csv("E:/assignment data/crime/2020-10-merseyside-street.csv", show_col_types = FALSE)
cd202011 = read_csv("E:/assignment data/crime/2020-11-merseyside-street.csv", show_col_types = FALSE)
cd202012 = read_csv("E:/assignment data/crime/2020-12-merseyside-street.csv", show_col_types = FALSE)
cd202101 = read_csv("E:/assignment data/crime/2021-01-merseyside-street.csv", show_col_types = FALSE)
cd202102 = read_csv("E:/assignment data/crime/2021-02-merseyside-street.csv", show_col_types = FALSE)
cd202103 = read_csv("E:/assignment data/crime/2021-03-merseyside-street.csv", show_col_types = FALSE)
cd202104 = read_csv("E:/assignment data/crime/2021-04-merseyside-street.csv", show_col_types = FALSE)
cd202105 = read_csv("E:/assignment data/crime/2021-05-merseyside-street.csv", show_col_types = FALSE)
cd202106 = read_csv("E:/assignment data/crime/2021-06-merseyside-street.csv", show_col_types = FALSE)
cd202107 = read_csv("E:/assignment data/crime/2021-07-merseyside-street.csv", show_col_types = FALSE)
cd202108 = read_csv("E:/assignment data/crime/2021-08-merseyside-street.csv", show_col_types = FALSE)
cd202109 = read_csv("E:/assignment data/crime/2021-09-merseyside-street.csv", show_col_types = FALSE)
cd202110 = read_csv("E:/assignment data/crime/2021-10-merseyside-street.csv", show_col_types = FALSE)
cd202111 = read_csv("E:/assignment data/crime/2021-11-merseyside-street.csv", show_col_types = FALSE)
cd202112 = read_csv("E:/assignment data/crime/2021-12-merseyside-street.csv", show_col_types = FALSE)

crimedata = cd201906 %>%  add_row(cd2019006 ) %>%  add_row(cd201907) %>%  add_row(cd201908) %>%   add_row(cd201909) %>%  add_row(cd201910) %>%
  add_row(cd201911) %>%  add_row(cd201912) %>% add_row(cd202001) %>%   add_row(cd202002) %>%   add_row(cd202003) %>%   add_row(cd202004) %>%
  add_row(cd202005) %>%   add_row(cd202006) %>%   add_row(cd202007) %>%   add_row(cd202008) %>%   add_row(cd202009) %>%
  add_row(cd202010) %>%   add_row(cd202011) %>%   add_row(cd202012) %>%   add_row(cd202101) %>%   add_row(cd202102) %>% 
  add_row(cd202103) %>%   add_row(cd202104) %>%   add_row(cd202105) %>%   add_row(cd202106) %>%   add_row(cd202107) %>%
  add_row(cd202108) %>%   add_row(cd202109) %>%   add_row(cd202110) %>%  add_row(cd202111) %>%   add_row(cd202112) %>% 
  mutate(Year=substring(Month, 1,4)) %>% 
  rename(lsoa11cd="LSOA code",CrimeType="Crime type") %>% 
  select(lsoa11cd,Year,CrimeType)
  
  
Towns = read_csv("E:/assignment data/cleandata/Towns.csv")
lsoa = read_csv("E:/assignment data/Postcode to LSOA.csv")

lsoa  = lsoa %>% 
  mutate(shortPostcode = str_trim(substring(pcds, 1,4))) %>% 
  left_join(Towns,by="shortPostcode") %>% 
  filter(County=="MERSEYSIDE"|County=="GREATER MANCHESTER") %>%
  group_by(lsoa11cd) %>% 
  filter(row_number()==1) %>% 
  select(lsoa11cd,shortPostcode,Town,District,County)
lsoa


cleanCrimes = crimedata %>% 
  left_join(lsoa,by="lsoa11cd")%>% 
  group_by(shortPostcode,Year,CrimeType)  %>% 
  select(shortPostcode,Year,CrimeType) %>% 
  tally()

write.csv(cleanCrimes, "E:/sem4_Data_science/cleanCrime.csv")





# SCHOOL DATA CLEANING

liverpoolSchool16 = read_csv("E:/assignment data/liverpoolSchool16.csv", show_col_types = FALSE) %>% 
  mutate(Year = 2016)
liverpoolSchool17 = read_csv("E:/assignment data/liverpoolSchool17.csv", show_col_types = FALSE) %>% 
  mutate(Year = 2017)
liverpoolSchool18 = read_csv("E:/assignment data/liverpoolSchool18.csv", show_col_types = FALSE) %>% 
  mutate(Year = 2018)
liverpoolSchool19 = read_csv("E:/assignment data/liverpoolSchool19.csv", show_col_types = FALSE) %>% 
  mutate(Year = 2019)
manchesterSchool16 = read_csv("E:/assignment data/manchesterSchool16.csv", show_col_types = FALSE) %>% 
  mutate(Year = 2016)
manchesterSchool17 = read_csv("E:/assignment data/manchesterSchool17.csv", show_col_types = FALSE) %>% 
  mutate(Year = 2017)
manchesterSchool18 = read_csv("E:/assignment data/manchesterSchool18.csv", show_col_types = FALSE)%>% 
  mutate(Year = 2018)
manchesterSchool19 = read_csv("E:/assignment data/manchesterSchool19.csv", show_col_types = FALSE)%>% 
  mutate(Year = 2019)


liverpoolSchool16 = select(liverpoolSchool16, Year, PCODE, SCHNAME, ATT8SCR)
liverpoolSchool17 = select(liverpoolSchool17, Year, PCODE, SCHNAME, ATT8SCR)
liverpoolSchool18 = select(liverpoolSchool18, Year, PCODE, SCHNAME, ATT8SCR)
liverpoolSchool19 = select(liverpoolSchool19, Year, PCODE, SCHNAME, ATT8SCR)
manchesterSchool16 = select(manchesterSchool16, Year, PCODE, SCHNAME, ATT8SCR)
manchesterSchool17 = select(manchesterSchool17, Year, PCODE, SCHNAME, ATT8SCR)
manchesterSchool18 = select(manchesterSchool18, Year, PCODE, SCHNAME, ATT8SCR)
manchesterSchool19 = select(manchesterSchool19, Year, PCODE, SCHNAME, ATT8SCR)


schoolData = manchesterSchool19 %>% 
  add_row(manchesterSchool18) %>% 
  add_row(manchesterSchool17) %>% 
  add_row(manchesterSchool16) %>% 
  add_row(liverpoolSchool19) %>% 
  add_row(liverpoolSchool18) %>% 
  add_row(liverpoolSchool17) %>% 
  add_row(liverpoolSchool16) %>% 
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()
colnames(schoolData) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")

write.csv(schoolData, "E:/sem4_Data_science/cleanSchool.csv") 

# School data cleaning seperatly for Manchester and Liverpool

manchesterSchool = manchesterSchool19 %>% 
  add_row(manchesterSchool18) %>%
  add_row(manchesterSchool17) %>%
  add_row(manchesterSchool16) %>%
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()

colnames(manchesterSchool) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")
write.csv(manchesterSchool, "E:/sem4_Data_science/manchester_school.csv") 


liverpoolSchool= liverpoolSchool19 %>% 
  add_row(liverpoolSchool18) %>% 
  add_row(liverpoolSchool17) %>% 
  add_row(liverpoolSchool16) %>% 
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()
colnames(liverpoolSchool) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")
write.csv(liverpoolSchool, "E:/sem4_Data_science/liverpoolSchool.csv") 

