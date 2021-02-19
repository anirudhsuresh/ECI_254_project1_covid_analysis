library(tidyverse)
library(data.table)

#data<-fread('Data/COVID-19_Case_Surveillance_Public_Use_Data.csv')
data<-read_csv('data/COVID-19.CSV')
#use the above



# mydata<-data[data$cdc_case_earliest_dt>'2020-11-30',]

mydata<-data

# new data frome
new_data<- mydata%>%
  select(cdc_case_earliest_dt,sex, age_group, race_ethnicity_combined, medcond_yn, icu_yn,death_yn, hosp_yn)


#-------------------------------------------------------------------------------------------------------------------------------------------------

# 
new_data$medcond_yn[new_data$medcond_yn=='Missing']<-NA
new_data$icu_yn[new_data$icu_yn=='Missing']<-NA
new_data$death_yn[new_data$death_yn=='Missing']<-NA
new_data$hosp_yn[new_data$hosp_yn=='Missing']<-NA
new_data$age_group[new_data$age_group=='Missing']<-NA


# defining a new data frame
mydata1<-new_data

install.packages('ggeasy')
library('ggeasy')
install.packages('ggpmisc')
library('ggpmisc')

holiday<-read_csv('data/holidays.csv')
holiday
#--------------------------------------------------------------------------------------------------------------------------------------
# time series for all covid cases from jan 2020 till jan 2021 ####
d_covid<-mydata1%>%filter(!is.na(hosp_yn))%>%filter(!is.na(icu_yn))%>% filter(!is.na(death_yn))%>%
  group_by(cdc_case_earliest_dt)%>%summarize(count=n())

time_covid<- d_covid%>%left_join(holiday, by= 'cdc_case_earliest_dt')

d_covid%>% ggplot(aes(x=cdc_case_earliest_dt,y=count))+geom_line(color='red')+labs(title = 'Variation of COVID-19 cases In the US over time', x='Month', y='Number of Cases')+
  geom_smooth(method = 'lm', se=FALSE, linetype='dashed', color='blue')+theme_bw()+ggeasy::easy_center_title()




# trying wrt to holidays
base::plot(d_covid$cdc_case_earliest_dt ,d_covid$count,type = "h", col = "gray",lwd=0.5, 
           xlab = "Dates", ylab = "New Cases")
lines(time_covid$cdc_case_earliest_d,time_covid$count, lwd=1.2)


#-------------------------------------------------------------------------------------------------------------------------------------------#

# all races analysis for covid cases ####
race_df<-mydata1%>%filter(!is.na(race_ethnicity_combined)) %>%group_by(race_ethnicity_combined)%>%summarize(count=n())%>%
  mutate(percentage=prop.table(count))%>% arrange(desc(percentage))
x<-race_df$race_ethnicity_combined
race_df$race_ethnicity_combined<-gsub("(.*),.*", "\\1", x)
# removing missing and unknown
x<-race_df$race_ethnicity_combined
race_df$race_ethnicity_combined<-gsub("(.*)/.*", "\\1", x)
race_df<-as_tibble(race_df)
race_df %>% filter(race_df$race_ethnicity_combined %in%c("American Indian","Asian","Black","Hispanic","Multiple","Native Hawaiian","White"))

race_df<-race_df%>% filter(race_ethnicity_combined != c("Unknown"))
race_df<-race_df %>% filter(race_ethnicity_combined != c("Missing"))

race_df%>% ggplot(aes(reorder(race_ethnicity_combined, count),count,label = scales::percent(percentage)))+geom_bar(stat="identity",fill='#FF6666')+
  labs(title = 'Variation of total COVID-19 cases In the US for Different Races', x='Races', y='Total Number of Cases')+
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,                             # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(axis.text.y = element_blank())+
  theme_bw()+ggeasy::easy_center_title()+theme(axis.text.y = element_blank())
#-----------------------------------------------------------------------------------

# cumulative ages and races for covid cases ####
#All age groups cases for all races 
age_df<-mydata1%>%filter(!is.na(age_group)) %>%group_by(age_group)%>%summarize(count=n())%>% mutate(percentage=prop.table(count))
age_df%>% ggplot(aes(age_group,count,label = scales::percent(percentage)))+geom_bar(stat="identity",fill='SteelBlue')+
  labs(title = 'Variation of total COVID-19 cases In the US for Different Age Groups', x='Age Groups', y='Total Number of Cases')+
  theme_bw()+
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,                             # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(axis.text.y = element_blank())+
  ggeasy::easy_center_title()
  

#----------------------------------------------------------------------------------------------------------#
## people who have medical conditions and are in the hospitaliza by race
host_data<-mydata1%>%group_by(race_ethnicity_combined, hosp_yn)%>%filter(!is.na(race_ethnicity_combined))%>%
  summarize(count=n())%>%filter(hosp_yn=='Yes')%>% mutate(percentage=count/sum(count))%>%arrange(desc(count))

host_data
# Hospitalization by race ####

# removing everything after  comma
x<-host_data$race_ethnicity_combined
host_data$race_ethnicity_combined<-gsub("(.*),.*", "\\1", x)
# removing before '/'
x<-host_data$race_ethnicity_combined
host_data$race_ethnicity_combined<-gsub("(.*)/.*", "\\1", x)
# removing missing and unknown
host_data<-as_tibble(host_data)
host_data %>% filter(host_data$race_ethnicity_combined %in% c("American Indian","Asian","Black","Hispanic","Multiple","Native Hawaiian","White"))

host_data<-host_data %>% filter(race_ethnicity_combined != c("Unknown"))
host_data<-host_data %>% filter(race_ethnicity_combined != c("Missing"))

plot_m<-host_data %>% ggplot(aes(reorder(host_data$count,race_ethnicity_combined,labels = scales::percent),x=host_data$count,fill=host_data$race_ethnicity_combined))+
  geom_bar(stat="identity",position = "dodge",width=0.2)+
  theme_classic(base_size = 18)+
  ylab("Different Races")+ # for the x axis label
  xlab("Number of Cases ")+ # for the y axis label
  labs(fill="Races",title='COVID 19 Hospitalization Cases by Race')+ geom_text(aes(label = race_ethnicity_combined), hjust=-0.001,vjust =0.001)
plot_m+theme(axis.text.y = element_blank())+ theme(legend.position = 'none')+ ggeasy::easy_center_title()


#----------------------------------------------------------------------------------------------------------------
# new analysis hospi by age group 
# people who have medical conditions and are in the hospitalization by race
age_host_data<-mydata1%>%group_by(age_group, hosp_yn)%>%filter(!is.na(age_group))%>%
  summarize(count=n())%>%filter(hosp_yn=='Yes')%>%arrange(desc(count))

age_host_data


age_host_data
# Hospitalization by race ####


plot_m<-age_host_data %>% ggplot(aes(reorder(age_host_data$count,age_group),x=age_host_data$count,fill=age_host_data$age_group))+
  geom_bar(stat="identity",position = "dodge",width=0.2)+
  theme_classic(base_size = 18)+
  ylab("Different Age Groups")+ # for the x axis label
  xlab("Number of Cases ")+ # for the y axis label
  labs(fill="Age Group",title='COVID 19 Hospitalization Cases by Age group')+ geom_text(aes(label =age_group), hjust=-0.001,vjust =0.001)
plot_m+theme(axis.text.y = element_blank())+ theme(legend.position = 'none')+ggeasy::easy_center_title()

#------------------------------------------------------------------------------------------------------------#
#1
## people who have medical conditions and are in the ICU by race ####
medcond_data<-mydata1%>% filter(!is.na(icu_yn))%>%group_by(race_ethnicity_combined, icu_yn, medcond_yn)%>%summarize(count=n())%>%
  filter(icu_yn=='Yes' & medcond_yn=='Yes')
# removing everything after  comma
x<-medcond_data$race_ethnicity_combined
medcond_data$race_ethnicity_combined<-gsub("(.*),.*", "\\1", x)
#removing everything after /
x<-medcond_data$race_ethnicity_combined
medcond_data$race_ethnicity_combined<-gsub("(.*)/.*", "\\1", x)
# removing missing and unknown
medcond_data<-as_tibble(medcond_data)
medcond_data %>% filter(medcond_data$race_ethnicity_combined %in%c("American Indian","Asian","Black","Hispanic","Multiple","Native Hawaiian","White"))

medcond_data<-medcond_data %>% filter(race_ethnicity_combined != c("Unknown"))
medcond_data<-medcond_data %>% filter(race_ethnicity_combined != c("Missing"))
medcond_data<-medcond_data%>%mutate(perc=prop.table(count))
medcond_data

plot_m<-medcond_data %>% ggplot(aes(reorder(medcond_data$count,race_ethnicity_combined),x=medcond_data$count,fill=medcond_data$race_ethnicity_combined, label = scales::percent(perc)))+
  geom_bar(stat="identity",position = "dodge",width=0.2)+
  theme_classic(base_size = 18)+
  ylab("Different Races")+ 
  xlab("Number of ICU Cases ")+ 
  labs(fill="Races",title='Patients with Previous Medicial Conditions Ended Up In ICUs') 
plot_m+theme_bw()+theme(axis.text.y = element_blank())+ theme(legend.position = 'left')+ggeasy::easy_center_title()+
  geom_text(position = position_dodge(width = .9),    
           vjust = 0.4,                             
           size = 3,
           hjust=-0.05)

#geom_text(aes(label = race_ethnicity_combined), hjust=-0.05,vjust =0.4)
#-------------------------------------------------------------------------------------------------------------------------------------------------
#2
## people who dont have medical conditions and are in the ICU by race ####
medcond_data<-mydata1%>% filter(!is.na(icu_yn))%>%group_by(race_ethnicity_combined, icu_yn, medcond_yn)%>%summarize(count=n())%>%
  filter(icu_yn=='Yes' & medcond_yn=='No')

medcond_data
# removing everything after  comma

x<-medcond_data$race_ethnicity_combined
medcond_data$race_ethnicity_combined<-gsub("(.*),.*", "\\1", x)
x<-medcond_data$race_ethnicity_combined
medcond_data$race_ethnicity_combined<-gsub("(.*)/.*", "\\1", x)
#
medcond_data<-as_tibble(medcond_data)
medcond_data %>% filter(medcond_data$race_ethnicity_combined %in%c("American Indian","Asian","Black","Hispanic","Multiple","Native Hawaiian","White"))

medcond_data<-medcond_data %>% filter(race_ethnicity_combined != c("Unknown"))
medcond_data<-medcond_data %>% filter(race_ethnicity_combined != c("Missing"))
medcond_data<-medcond_data%>% mutate(perc=prop.table(count))

medcond_data
plot_m<-medcond_data %>% ggplot(aes(reorder(medcond_data$count,race_ethnicity_combined),x=medcond_data$count,fill=medcond_data$race_ethnicity_combined, label = scales::percent(perc)))+
  geom_bar(stat="identity",position = "dodge",width=0.2)+
  theme_classic(base_size = 18)+
  ylab("Race/Ethnicity")+ 
  xlab("Total Number of Cases ")+ 
  labs(fill="Races",title='Patients with no Previous Medicial Conditions Ended Up In ICUs')
plot_m+ theme_bw()+ theme(axis.text.y = element_blank())+theme(legend.position =  'left')+ggeasy::easy_center_title()+
  geom_text(position = position_dodge(width = .9),    
            vjust = 0.4,                             
            size = 3,
            hjust=-0.05)+scale_fill_discrete()

#+ geom_text(aes(label = race_ethnicity_combined), hjust=-0.05,vjust =0.4)
#-------------------------------------------------------------------------------------------------------------------#
#3
# hospital cases men vs women ####
hos_cases<-mydata1 %>%filter(!is.na(hosp_yn))%>%filter(!is.na(sex)) %>%group_by(hosp_yn,sex)%>%summarize(count=n())%>%
  mutate(percentage=prop.table(count))
hos_cases
# removing the missing other and unknown sexes
hos_cases<-hos_cases%>%filter(sex %in% c('Female','Male'))
hos_cases<-hos_cases%>%filter(hosp_yn %in% c('No','Yes'))
hos_cases%>% ggplot(aes(sex,count,fill=hosp_yn, label = scales::percent(percentage)
))+geom_bar(stat="identity",position ='dodge')+
  theme_classic(base_size = 18)+
  xlab("Gender")+ 
  ylab("Percentage")+ 
  labs(fill="Hospitilzation",title='Total Percentage of COVID-19 Cases Based on Gender and Hospitalization Records')+
  geom_text(position = position_dodge(width = .9),    
                                        vjust = -0.5,                             
                                        size = 3) + 
  scale_y_continuous(labels = scales::percent)+theme(axis.text.y = element_blank())+ggeasy::easy_center_title()




#-------------------------------------------------------------------------------------------------------------------------------------------------
#4
#icu cases men women ####
icu_cases<-mydata1 %>%filter(!is.na(hosp_yn))%>%filter(!is.na(sex)) %>%group_by(icu_yn,sex)%>%summarize(count=n())%>%
  mutate(percentage=prop.table(count))
# removing the missing other and unknown sexes
icu_cases<-icu_cases%>%filter(sex %in% c('Female','Male'))
icu_cases<-icu_cases%>%filter(icu_yn %in% c('No','Yes'))
icu_cases%>% ggplot(aes(sex,count,fill=icu_yn,label = scales::percent(percentage)))+geom_bar(stat="identity",position ='dodge')+
  theme_classic(base_size = 18)+
  xlab("Gender")+ 
  ylab("Percentage")+ 
  labs(fill="ICU")+geom_text(position = position_dodge(width = .9),    
                             vjust = -0.5,                             
                             size = 3) + labs(title = 'Total Percentage of COVID-19 Cases Based on Gender and Intensive Care Unit Records')
  scale_y_continuous(labels = scales::percent)+theme(axis.text.y = element_blank())+ggeasy::easy_center_title()

#----------------------------------------------------------------------------------------------------------####
#5
#race analysis for each month
race_df<-mydata1%>%filter(!is.na(race_ethnicity_combined)) %>% mutate(date_by_month= strftime(cdc_case_earliest_dt,'%m'))%>% 
  group_by(date_by_month, race_ethnicity_combined)%>%
  summarize(count=n())

race_df
x<-race_df$race_ethnicity_combined
race_df$race_ethnicity_combined<-gsub("(.*),.*", "\\1", x)
x<-race_df$race_ethnicity_combined
race_df$race_ethnicity_combined<-gsub("(.*)/.*", "\\1", x)
# removing missing and unknown
race_df<-as_tibble(race_df)
race_df %>% filter(race_df$race_ethnicity_combined %in% 
                     c("American Indian","Asian","Black","Hispanic","Multiple","Native Hawaiian","White"))

race_df<-race_df%>% filter(race_ethnicity_combined != c("Unknown"))
race_df<-race_df %>% filter(race_ethnicity_combined != c("Missing"))

race_df%>% ggplot(aes(y=race_ethnicity_combined,x= count,fill=race_ethnicity_combined))+
  geom_bar(stat="identity")+facet_wrap(~date_by_month)+labs(title = 'Total Number of COVID-19 Cases over the year of 2020 Bases on Races/Ethnicities ' , x='Total Number of Cases Per Month', y='Race/Ethnicity')+
  ggeasy::easy_center_title()+theme(legend.position = 'none')

#-------------------------------------------------------------------------------------------------------------
#6
#age analysis for each month
age_df<-mydata1%>%filter(!is.na(race_ethnicity_combined)) %>% filter(!is.na(age_group))%>%mutate(date_by_month= strftime(cdc_case_earliest_dt,'%m'))%>% 
  group_by(date_by_month,age_group)%>%
  summarize(count=n())

age_df


age_df%>% ggplot(aes(y=age_group,x= count,fill=age_group))+
  geom_bar(stat="identity")+facet_wrap(~date_by_month)+labs(title = 'Total Number of COVID-19 Cases over the Year of 2020 for Diffferent Age Groups' , x='Total Number of Cases Per Month', y='Age Groups')+
  ggeasy::easy_center_title()+theme(legend.position = 'none')
  

                                       