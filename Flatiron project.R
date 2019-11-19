library(readxl)
library(tidyverse)
data<-readxl::excel_sheets("flatiron_qs_orders_admins_july_16.xlsx") %>% map(~readxl::read_excel("flatiron_qs_orders_admins_july_16.xlsx", .x)) %>% set_names(excel_sheets("flatiron_qs_orders_admins_july_16.xlsx"))

write.csv(data$Orders, file = "Orders.csv")
write.csv(data$Admins, file = "Admins.csv")
write.csv(data$Demographics, file = "Demographics.csv")
write.csv(data$Patients, file = "Patients.csv")
write.csv(data$Practices, file = "Practices.csv")

Orders<-read.csv("Orders.csv", stringsAsFactors=FALSE)
Admins<-read.csv("Admins.csv", stringsAsFactors=FALSE)
Demographics<-read.csv("Demographics.csv", stringsAsFactors=FALSE)
Patients<-read.csv("Patients.csv", stringsAsFactors=FALSE)
Practices<-read.csv("Practices.csv", stringsAsFactors=FALSE)


DF<-full_join(Patients, Demographics, by='patient_id')
DF2<-full_join(Admins, Orders, by=c("patient_id", "external_patient_id", "order_id"))



#1.	What is the average time elapsed between a patient's initial diagnosis date and a patient's first treatment? Does this time vary by gender?

#To determeine the elapsed time, need to find the differences between diagnosis data and (drug) administration date, then compute the average:
DF2$my_administered_date <- as.Date(DF2$administered_date,format="%d-%b-%Y")
#Administed datesare in two different formats, have to account for both and merge the output data:
DF2$b<-as.Date(DF2$administered_date,format="%Y-%m-%d")
DF2<-DF2 %>% mutate(my_administered_date = coalesce(my_administered_date,b))
DF2$b<-NULL
DF$my_diagnosis_date <- as.Date(DF$diagnosis_date,format="%d-%b-%Y")

#since we have multiple drug administration dates, it is neccessary to find the first date for each patient from the admin data and join this with the patient data
first_treatment<-DF2 %>% 
  group_by(patient_id) %>% 
  arrange(my_administered_date) %>%
  summarise(first_administered_date = first(my_administered_date),  n = n()) 

#Now join the first treatment dates for patients with our original data that includes diagnosis dates, find the differences, and compute the mean: 
DF<-right_join(DF, first_treatment, by='patient_id')

DF<-mutate(DF, difference=first_administered_date-my_diagnosis_date) 
DF<-DF%>%mutate(difference=as.numeric(difference))
            
            
mean(DF$difference, na.rm = TRUE)
#The mean between the first date a patient was administered medicine (assuming this is the first treatment) and the patient's inital diagnosis is 14037.99 days.

DF %>%
  group_by(gender) %>%
  summarise(Mean = mean(difference, na.rm=TRUE))
#The mean between the first date a patient was administered medicine (assuming this is the first treatment) and the patient's inital diagnosis is 15385 days for females and 12688 for males.
#We couldalternatively use the order_date to make these calculations as they appear the same as the adminstered dates, but have fewer NAs




#2. How many patients are on nivolumab from 2012 to 2016?
DATE1<-as.Date("2012-01-01")
DATE2<-as.Date("2016-01-01")
DATES <- subset(DF2, my_administered_date>=DATE1 & my_administered_date<DATE2)
newvar<-DATES %>%
  group_by(patient_id) %>%
  filter(drug_name=="nivolumab")%>%
  summarise(count = n_distinct(patient_id))

newvar%>%summarise(count = n_distinct(patient_id))
#There were 62 patients on nivolumab from 2012 until 2016





# 3.	Using the following risk stratification rules, please summarize the number of high, medium, and low risk patients.

#HighRisk = Female; any???age; NON_WHITE OR Male; >= 70; NON_WHITE 
#MediumRisk = Female; >=75, WHITE OR Male; <70; NON_WHITE 
#LowRisk = Female; < 75, WHITE OR Male; any???age; WHITE

DF3<-DF%>%
  mutate(risk=ifelse(DF$age>=70  & DF$gender=="male" & DF$race=="NON_WHITE", "HighRisk",
                ifelse(DF$gender=="female" & DF$race=="NON_WHITE", "HighRisk",
                       ifelse(DF$gender=="female" & DF$age>=75 & DF$race=="WHITE", "MediumRisk",
                              ifelse(DF$gender=="male" & DF$age<70 & DF$race=="NON_WHITE", "MediumRisk",
                                     ifelse(DF$gender=="female" & DF$age<75 & DF$race=="WHITE", "Low_Risk",
                                            ifelse(DF$gender=="male" & DF$race=="WHITE", "Low_Risk", NA)))))))


DF4<-DF3%>%select(patient_id, risk, gender, race)
DF4<-distinct(DF4)

table(DF4$risk)
table(DF4$risk, DF4$gender, DF4$race)
table2<-table(DF4$risk) 
prop.table(table2)
#COUNT
#HighRisk   Low_Risk MediumRisk 
#   31         40         24 

#PROPORTIONS
#HighRisk   Low_Risk MediumRisk 
#0.3263158  0.4210526  0.2526316 

#COUNT BY RACE AND SEX
#, ,  = NON_WHITE
#female male unknown
#HighRisk       23    8       0
#Low_Risk        0    0       0
#MediumRisk      0    8       0

#, ,  = WHITE
#female male unknown
#HighRisk        0    0       0
#Low_Risk       12   28       0
#MediumRisk     16    0       0


DF$risk2<- ifelse (DF$age>=70  & DF$gender=="male" & DF$race=="NON_WHITE", "HighRisk", NA){
  print("HighRisk")
} else if (DF$gender=="female" & DF$race=="NON_WHITE") {
  "HighRisk"
} else if ( test_expression3) {
  statement3
}



#4.	Please create a visualization that could be used to help a medical researcher understand how drug prevalence has changed over time
#time series by year and time series based on practiceid and pracice_type and drug type
library(ggplot2)
ggplot(DF2, aes(my_administered_date)) + geom_histogram(binwidth=80) + ggtitle("Drug Administration")
ggplot(DF2, aes(my_administered_date)) + geom_histogram(aes(fill=drug_name)) + ggtitle("Drug Administration")
ggplot(DF2, aes(my_administered_date)) + geom_histogram(binwidth=140, aes(fill=drug_name)) + ggtitle("Drug Administration") + scale_x_date(limits = as.Date(c('2010-01-01','2016-01-01')))
DF2$year<-lubridate::year(DF2$my_administered_date)                                                                                                                                         
ggplot(DF2, aes(year)) + geom_histogram(aes(fill=drug_name)) + ggtitle("Drug Administration")
ggplot(DF2, aes(year)) + geom_histogram(aes(fill=drug_name)) + ggtitle("Drug Administration") + scale_x_continuous(limits = c(2010,2016))
ggplot(DF2, aes(year)) + geom_histogram(binwidth = 0.5, aes(fill=drug_name)) + ggtitle("Drug Administration") + scale_x_continuous(limits = c(2010,2016))

ggplot(DF2, aes(year)) + geom_histogram(binwidth = 0.5, aes(fill=Practices)) + ggtitle("Drug Administration") + scale_x_continuous(limits = c(2010,2016))
#Add practice type variable to DF2 based of DF external_patient_id

ggplot(DF2,aes(x=year,color=drug_name)) + geom_line(stat='count', size=1) + scale_x_continuous(limits = c(2010,2016))

ggplot(DF2,aes(x=year,fill=drug_name)) + geom_bar(stat='count', position = "dodge", width = .8) + scale_x_continuous(limits = c(2010,2016))






DF$Risk <- NA
DF$Risk[which((DF$age>=70  & DF$gender=="male" & DF$race=="NON_WHITE")|(DF$gender=="female" & DF$race=="NON_WHITE"))] <- "HighRisk"
DF$Risk[which((DF$gender=="female" & DF$age>=75 & DF$race=="WHITE")|(DF$gender=="male" & DF$age<70 & DF$race=="NON_WHITE"))] <- "MediumRisk"
DF$Risk[which((DF$gender=="female" & DF$age<75 & DF$race=="WHITE")|(DF$gender=="male" & DF$race=="WHITE"))] <- "Lowrisk"