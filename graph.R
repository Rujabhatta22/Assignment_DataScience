library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(ggrepel)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
install.packages("ggforce")
library(ggforce) 



getwd()
setwd("E:/")
setwd("/sem4_Data_science")

euro <- dollar_format(prefix = "\u20ac", big.mark = ",")
options(scipen=999)

#-------------------------graph of broadband speed---------------------
Towns = read_csv("Towns.csv")%>% 
  select(shortPostcode, Town, District, County)

Towns=Towns [!duplicated(Towns$District),]
BroadbandSpeedsclean=read_csv("cleanBroadbandSpeeds.csv")


broadband=Towns %>% 
  mutate(shortPostcode = str_trim(str_sub(PostCode, -4,-1))) %>% 
  left_join(BroardbandSpeeds, by = "shortPostcode") %>% 
  na.omit()


ggplot(broadband,aes(y=Town)) +
  labs(x="Speeds (Mbits/s)",y="Town",title="GREATER MANCHESTER Broadband Speeds")+
  geom_bar(data=filter(broadband,County=="GREATER MANCHESTER"),aes(x=MaxDownload,fill="Maximum"),stat="Identity")+
  geom_bar(data=filter(broadband,County=="GREATER MANCHESTER"),aes(x=AverageDownload,fill="Average"),stat="Identity")+
  guides(fill=guide_legend("Download Speeds"))

#---------------MERSEYSIDE Broadband Speeds---------------
ggplot(Broadband,aes(y=Town)) +
  labs(x="Speeds (Mbits/s)",y="Town",title="MERSEYSIDE Broadband Speeds")+
  geom_bar(data=filter(Broadband,County=="MERSEYSIDE"),aes(x=MaxDownload,fill="maximum"),stat="Identity")+
  geom_bar(data=filter(Broadband,County=="MERSEYSIDE"),aes(x=AverageDownload,fill="Average"),stat="Identity")+
  guides(fill=guide_legend("Download Speeds"))

#---------------Average download speed---------------
ggplot(broadband,aes(x=AverageDownload,y=District,fill=District))+
  geom_boxplot(outlier.colour="red")+
  scale_x_continuous(limits=c(25,70), breaks=seq(25,70,10)) +
  labs(y="District",x="Speeds (Mbits/s)",title="Average Download Speeds")

#--------------broadband end-----------------------------



#---------------graph of Average house price ------------------

HousePricesclean=read_csv("HousePricesclean.csv")

HousePricesclean=HousePricesclean %>% 
  left_join(Towns, by ="shortPostcode")

House_town = HousePricesclean %>% 
            filter(County=="GREATER MANCHESTER"|County=="MERSEYSIDE") %>% 
  group_by(Town,District,County) %>% 
  summarise(AveragePrice= mean(Price)) %>% 
  ungroup(Town,District,County) %>%
              na.omit()

# BOXPLOT Average house prices by district (2019-2021)
House_town %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = AveragePrice, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2019-2021 house prices by district")



# BARGRAPH houseprices by district (2021)
HousePricesclean %>% 
  filter(Year == 2021) %>% 
  group_by(District) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = District, y = AveragePrice)) +
  geom_bar(position = "stack",stat = "identity", fill = "cornflowerblue") +
  scale_y_continuous(limits=c(0,5000000),breaks = seq(0, 5000000, 30000), 
                     label = euro) +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = -0.25) +
  labs(title = "2021 Average house prices by district") +
  coord_flip()


#LINEGRAPH Average house prices by year (2019-2021)
HousePricesclean %>% 
  group_by(Year) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = Year, y = AveragePrice)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = -0.85) +
  scale_y_continuous(breaks = seq(0, 300000, 5000), 
                     label = euro) +
  scale_x_continuous(breaks = 2019:2021) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "2019-2021 Average house prices by year")


# BOXPLOT Average house prices by district (2019-2021)
HousePricesclean %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Price, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2019-2021 house prices by district")



# BOXPLOT Average house prices by district (2021)
HousePricesclean %>% 
  filter(Year == 2021) %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Price, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2021 house prices by district")

#-------------end------------------------------------------

#-----------crime graph-------------------


crime_Data = read_csv("cleanCrimes.csv")



crimeData = crime_Data %>% 
  left_join(Towns, by = "shortPostcode") %>% 
  na.omit()


# Boxplot for 2019-2021 Drugs count by District
crimeData %>% 
  filter(CrimeType == "Drugs") %>% 
  ggplot(aes(x=District, y=n, fill=CrimeType)) + 
  geom_boxplot() +
  labs(title=" 2019-2021 Drugs count by District")+
  coord_flip()



# Piechart for 2021 Robbery by District
RobberyData <- crimeData %>% 
  filter(CrimeType=="Robbery", Year == 2021) %>%
  group_by(Town) %>%
  mutate(sumCount = sum(n)) %>% 
  ungroup() %>%
  mutate(perc =sumCount / sum(n)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>% 
  distinct(Town, sumCount, perc, labels) %>% 
  select(Town, sumCount, perc, labels)

DrugData <- crimeData %>% 
  filter(CrimeType=="Drugs", Year == 2021)
 




 
RobberyData %>% 
  ggplot(aes(x = "", y = perc, fill =Town)) +
  geom_col(color = "white") +
  geom_label(aes(label = labels),color="black",
             position = position_stack(vjust = .5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  theme_void()+
  labs(title="2021 Robbery by Town")





new_row <- c("Town" = "min", "sumCount" = 3, "perc" = 0.005405405,"labels"=0.54)
new_row1 <- c("Town" = "max", "sumCount" = 307, "perc" = 	
                0.553153153153153,"labels"=55.32)


data_robbery<-rbind(RobberyData,new_row,new_row1) %>% 
  select(sumCount,labels,perc)

df_robbery<-data_robbery[c("max","min","WIGAN"),]
radarchart(data_robbery)

data2 <- data_robbery                                          # Duplicate example data
data2 <- tibble::rownames_to_column(data_robbery, "Town") # Apply rownames_to_column
data2  


DLine<-ggplot(data= drug_manchester,aes(x=sumCount)+
                geom_line(aes(y=County)))


#------------


drug_manchester=DrugData %>% 
  filter(County=="GREATER MANCHESTER")

drug_merseyside=DrugData %>% 
  filter(County=="MERSEYSIDE")
  

plot(drug_manchester,type = "o",col = "red", xlab = "sumCount", ylab = "County", 
     main = "Rain fall chart")

lines(drug_merseyside, type = "o", col = "blue")

#--------------------------------right

ggplot(DrugData, aes(x = factor(n), y = Town, colour = County, group = County)) +
  geom_line()
  scale_x_continuous(limits=c(10,20,),breaks = seq(0, 5000000, 30000))
 

#-----------------school graph---------------

schoolData = read_csv('cleanSchool.csv', show_col_types = FALSE)

liverpoolSchool = read_csv("liverpoolSchool.csv")
manchesterSchoolData = read_csv("manchester_school.csv")


# Linegraph Average Attainment8Score by year
schoolData %>% 
  group_by(Year) %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = Year, y = AverageAttainment)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = AverageAttainment), 
            vjust = -0.85) +
  scale_x_continuous(breaks = 2016:2019) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "Average Attainment8Score by year")


# Boxplot of year 2016-2019 where Attainment8Score is greater than 30
schoolData %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2016-2019 Attainment8Score of Schools")


# Boxplot of year 2016-2019 where Attainment8Score is greater than 30 (LIVERPOOL SCHOOL ONLY)
liverpoolSchool %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="Average Attainment8Score of Liverpool Schools")



# Boxplot of year 2016-2019 where Attainment8Score is greater than 30 (MANCHESTER SCHOOL ONLY)
manchesterSchoolData %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2016-2019 Average Attainment8Score of Manchester Schools")


#--------------------radder graph of robbery------------------------
