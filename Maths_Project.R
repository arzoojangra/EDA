#libraries
library(ggplot2)
library(RColorBrewer)

#COLOUR LISTS
coul1 <- c("#660099", "#CC0099", "#00CCFF", "#00FF00", "#FFFF00", "#FF6600", "#FF99FF", "#FF0000")
coul2 <- c("#FF9966", "#33FF33", "#CC66FF", "#FFFF99", "#FF66CC", "#00CCFF", "#FF0033")
coul3 <- brewer.pal(5, "Set3")
coul4 <- brewer.pal(5, "Set2")
coul5 <- c("#FF3399", "#66CCFF", "#009966", "#FFFF00", "#FF3333","#FFCC99", "#66FFFF")

#reading the data and naming it as crimes_data
crimes_data<-read.csv("C:\\Users\\Jangra\\Downloads\\dstrCAW_1.csv")   
View(crimes_data)   

#renaming some columns 
colnames(crimes_data)[colnames(crimes_data) == 'Kidnapping.and.Abduction']<-"Kidnapping_Abduction"
colnames(crimes_data)[colnames(crimes_data) == 'Dowry.Deaths']<-"Dowry_Deaths"
colnames(crimes_data)[colnames(crimes_data) == 'Assault.on.women.with.intent.to.outrage.her.modesty']<-"Assaults"
colnames(crimes_data)[colnames(crimes_data) == 'Insult.to.modesty.of.Women']<-"Insults_to_modesty"
colnames(crimes_data)[colnames(crimes_data) == 'Cruelty.by.Husband.or.his.Relatives']<-"Cruelty_by_Husband"
colnames(crimes_data)[colnames(crimes_data) == 'Importation.of.Girls']<-"Importation"

#checking for the changes
View(crimes_data)    #changes verified

#storing state-wise total crimes in a variable
crime_total<-crimes_data[(crimes_data$DISTRICT=="TOTAL"),]
View(crime_total)

#removing totals for particular states
crimes_data<-crimes_data[!(crimes_data$DISTRICT=="TOTAL"),]
View(crimes_data)

#checking no. of rows and columns
nrow(crimes_data)
ncol(crimes_data)

#storing the names of crimes in crimes_names 
crimes_names <- c("Rape","Kidnapping_Abduction", "Dowry_Deaths", "Assaults", "Insults_to_modesty", "Cruelty_by_Husband", "Importation")

#checking the minimum of each crime from complete data
min(crimes_data$Rape)
min(crimes_data$Kidnapping_Abduction)
min(crimes_data$Dowry_Deaths)
min(crimes_data$Assaults)
min(crimes_data$Insults_to_modesty)
min(crimes_data$Cruelty_by_Husband)
min(crimes_data$Importation)
#minimum for each crime is 0

#checking the maximum of each crime from complete data
print('Maximum no. of Rapes:');max(crimes_data$Rape)  #568
print('Maximum no. of Kidnapping/Abduction:');max(crimes_data$Kidnapping_Abduction)   #492
print('Maximum no. of Dowry Deaths:');max(crimes_data$Dowry_Deaths)  #168
print('Maximum no. of Assaults:');max(crimes_data$Assaults)  #621
print('Maximum no. of Insults to Modesty');max(crimes_data$Insults_to_modesty)  #1257
print('Maximum no. of Cruelty by Husband:');max(crimes_data$Cruelty_by_Husband)  #3035
print('Maximum no. of Importation:');max(crimes_data$Importation)  #60

y = c(max(crimes_data$Rape), max(crimes_data$Kidnapping_Abduction), max(crimes_data$Dowry_Deaths), max(crimes_data$Assaults), max(crimes_data$Insults_to_modesty), max(crimes_data$Cruelty_by_Husband), max(crimes_data$Importation))

#plotting maximum of each crime 
par(mar = c(11,4,4,4))
barplot(height = y, name = crimes_names, col=coul3, ylab="Maximum no. of cases", 
        main="Different crimes V/S Maximum cases of the crime(2001-2012)", las=2, cex.axis=0.8, cex.names=1, font.axis=2, ylim = c(50,3200))

#The total number of crime cases recorded in the time period of 2001-2012
total <- sapply(crimes_data[, 4:10], sum)
total  
total <- sum(total, na.rm = FALSE)
total      #2047548

#separating data year-wise
crimes_2001<-crimes_data[(crimes_data$Year=="2001"),]
View(crimes_2001)

crimes_2002<-crimes_data[(crimes_data$Year=="2002"),]
View(crimes_2002)

crimes_2003<-crimes_data[(crimes_data$Year=="2003"),]
View(crimes_2003)

crimes_2004<-crimes_data[(crimes_data$Year=="2004"),]
View(crimes_2004)

crimes_2005<-crimes_data[(crimes_data$Year=="2005"),]
View(crimes_2005)

crimes_2006<-crimes_data[(crimes_data$Year=="2006"),]
View(crimes_2006)

crimes_2007<-crimes_data[(crimes_data$Year=="2007"),]
View(crimes_2007)

crimes_2008<-crimes_data[(crimes_data$Year=="2008"),]
View(crimes_2008)

crimes_2009<-crimes_data[(crimes_data$Year=="2009"),]
View(crimes_2009)

crimes_2010<-crimes_data[(crimes_data$Year=="2010"),]
View(crimes_2010)

crimes_2011<-crimes_data[(crimes_data$Year=="2011"),]
View(crimes_2011)

crimes_2012<-crimes_data[(crimes_data$Year=="2012"),]
View(crimes_2012)


#Ques1: Which year admitted the maximum number of crimes?

#finding the total number of crimes year-wise
gtotal_2001 <- sum(crimes_2001[, 4:10])
gtotal_2001    #total number of crimes recorded in 2001 : 130725

gtotal_2002 <- sum(crimes_2002[, 4:10])  #total number of crimes recorded in 2002 : 131112

gtotal_2003 <- sum(crimes_2003[, 4:10])  #total number of crimes recorded in 2003 : 131364

gtotal_2004 <- sum(crimes_2004[, 4:10])  #total number of crimes recorded in 2004 : 143615

gtotal_2005 <- sum(crimes_2005[, 4:10])  #total number of crimes recorded in 2005 : 143523

gtotal_2006 <- sum(crimes_2006[, 4:10])  #total number of crimes recorded in 2006 : 154158

gtotal_2007 <- sum(crimes_2007[, 4:10])  #total number of crimes recorded in 2007 : 174921

gtotal_2008 <- sum(crimes_2008[, 4:10])  #total number of crimes recorded in 2008 : 186616

gtotal_2009 <- sum(crimes_2009[, 4:10])  #total number of crimes recorded in 2009 : 194835

gtotal_2010 <- sum(crimes_2010[, 4:10])  #total number of crimes recorded in 2010 : 205009

gtotal_2011 <- sum(crimes_2011[, 4:10])  #total number of crimes recorded in 2011 : 219142

gtotal_2012 <- sum(crimes_2012[, 4:10])  #total number of crimes recorded in 2012 : 232528

#visualizing total crimes in each year
years <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)
years
totals <- c(gtotal_2001, gtotal_2002, gtotal_2003, gtotal_2004, gtotal_2005, gtotal_2006, gtotal_2007, gtotal_2008, gtotal_2009, gtotal_2010, gtotal_2011, gtotal_2012)
totals 
plot(years, totals, type = "b", pch = 19, col = coul1, xlab = "Years", ylab = "Total no. of Crimes",main = "Crimes V/S Years", ylim = c(100000,250000), )
#Ques1 finished!! Ans: Year 2012 recorded the maximum number of crimes: 232528 


#Ques2: Compare the crimes year-wise and state which crime happened maximum in each year.
#storing the total of each crime for particular year in a variable 
total_2001<-sapply(crime_total[crime_total$Year=='2001',][,4:10],sum)   
pie(total_2001, main = "Crime pie chart 2001",col = coul5)   #Cruelty_by_Husband is the crime which was recorded for the maximum no. of times!
print(total_2001)

total_2002<-sapply(crime_total[crime_total$Year=='2002',][,4:10],sum)
pie(total_2002, main = "Crime pie chart 2002",col = coul5)   #Cruelty_by_Husband is the crime which was recorded for the maximum no. of times!
print(total_2002)

total_2003<-sapply(crime_total[crime_total$Year=='2003',][,4:10],sum)
pie(total_2003, main = "Crime pie chart 2003",col = coul5)   #Cruelty_by_Husband is the crime which was recorded for the maximum no. of times!
print(total_2003)

total_2004<-sapply(crime_total[crime_total$Year=='2004',][,4:10],sum)
pie(total_2004, main = "Crime pie chart 2004",col = coul5)   #Cruelty_by_Husband is the crime which was recorded for the maximum no. of times!
print(total_2004)

total_2005<-sapply(crime_total[crime_total$Year=='2005',][,4:10],sum)
pie(total_2005, main = "Crime pie chart 2005",col = coul5)   #Cruelty_by_Husband is the crime which was recorded for the maximum no. of times!
print(total_2005)

total_2006<-sapply(crime_total[crime_total$Year=='2006',][,4:10],sum)
pie(total_2006, main = "Crime pie chart 2006",col = coul5)   #Cruelty_by_Husband is the crime which was recorded for the maximum no. of times!
print(total_2006)

total_2007<-sapply(crime_total[crime_total$Year=='2007',][,4:10],sum)
pie(total_2007, main = "Crime pie chart 2007",col = coul5)   #Cruelty_by_Husband is the crime which was recorded for the maximum no. of times!
print(total_2007)

total_2008<-sapply(crime_total[crime_total$Year=='2008',][,4:10],sum)
pie(total_2008, main = "Crime pie chart 2008",col = coul5)   #Cruelty_by_Husband is the crime which was recorded for the maximum no. of times!
print(total_2008)

total_2009<-sapply(crime_total[crime_total$Year=='2009',][,4:10],sum)
pie(total_2009, main = "Crime pie chart 2009",col = coul5)   #Cruelty_by_Husband is the crime which was recorded for the maximum no. of times!
print(total_2009)

total_2010<-sapply(crime_total[crime_total$Year=='2010',][,4:10],sum)
pie(total_2010, main = "Crime pie chart 2010",col = coul5)   #Cruelty_by_Husband is the crime which was recorded for the maximum no. of times!
print(total_2010)

total_2011<-sapply(crime_total[crime_total$Year=='2011',][,4:10],sum)
pie(total_2011, main = "Crime pie chart 2011",col = coul5)   #Cruelty_by_Husband is the crime which was recorded for the maximum no. of times!
print(total_2011)

total_2012<-sapply(crime_total[crime_total$Year=='2012',][,4:10],sum)
pie(total_2012, main = "Crime pie chart 2012",col = coul5)   #Cruelty_by_Husband is the crime which was recorded for the maximum no. of times!
print(total_2012)

total <- rbind(total_2001,total_2002,total_2003,total_2004,total_2005,total_2006,total_2007, 
               total_2008,total_2009,total_2010,total_2011,total_2012)
print(total_2001==max(total_2001))
max_crime<-max(total)
max_crime
max_crime_name<-colnames(total)[apply(total,1,which.max)]
max_crime_name
print(total)
#Ques2 finished!!


#Ques3: Examine which state witnessed the maximum no. of Dowry-Deaths in every particular year.
#For 2001
dowry_2001<-aggregate(x= crimes_2001$Dowry_Deaths,
                             by= list(crimes_2001$STATE.UT),
                             FUN=sum)
dowry_2001
states <- dowry_2001$Group.1
states
barplot(height = dowry_2001$x, name = states, col=coul4, ylab="Dowry Death Cases", 
        main="Dowry Deaths V/S States/UT (2001)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,2500))


#For 2002
dowry_2002<-aggregate(x= crimes_2002$Dowry_Deaths,
                             by= list(crimes_2002$STATE.UT),
                             FUN=sum)
dowry_2002
states <- dowry_2002$Group.1
barplot(height = dowry_2002$x, name = states, col=coul4, ylab="Dowry Death Cases", 
        main="Dowry Deaths V/S States/UT (2002)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,2000))


#For 2003
dowry_2003<-aggregate(x= crimes_2003$Dowry_Deaths,
                             by= list(crimes_2003$STATE.UT),
                             FUN=sum)
dowry_2003
states <- dowry_2003$Group.1
barplot(height = dowry_2003$x, name = states, col=coul4, ylab="Dowry Death Cases", 
        main="Dowry Deaths V/S States/UT (2003)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,1500))


#For 2004
dowry_2004<-aggregate(x= crimes_2004$Dowry_Deaths,
                             by= list(crimes_2004$STATE.UT),
                             FUN=sum)
dowry_2004
states <- dowry_2004$Group.1
barplot(height = dowry_2004$x, name = states, col=coul4, ylab="Dowry Death Cases", 
        main="Dowry Deaths V/S States/UT (2004)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,2000))


#For 2005
dowry_2005<-aggregate(x= crimes_2005$Dowry_Deaths,
                             by= list(crimes_2005$STATE.UT),
                             FUN=sum)
dowry_2005
states <- dowry_2005$Group.1
barplot(height = dowry_2005$x, name = states, col=coul4, ylab="Dowry Death Cases", 
        main="Dowry Deaths V/S States/UT (2005)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,2000))


#For 2006
dowry_2006<-aggregate(x= crimes_2006$Dowry_Deaths,
                             by= list(crimes_2006$STATE.UT),
                             FUN=sum)
dowry_2006
states <- dowry_2006$Group.1
barplot(height = dowry_2006$x, name = states, col=coul4, ylab="Dowry Death Cases", 
        main="Dowry Deaths V/S States/UT (2006)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,2000))


#For 2007
dowry_2007<-aggregate(x= crimes_2007$Dowry_Deaths,
                             by= list(crimes_2007$STATE.UT),
                             FUN=sum)
dowry_2007
states <- dowry_2007$Group.1
barplot(height = dowry_2007$x, name = states, col=coul4, ylab="Dowry Death Cases", 
        main="Dowry Deaths V/S States/UT (2007)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,2500))


#For 2008
dowry_2008<-aggregate(x= crimes_2008$Dowry_Deaths,
                             by= list(crimes_2008$STATE.UT),
                             FUN=sum)
dowry_2008
states <- dowry_2008$Group.1
barplot(height = dowry_2008$x, name = states, col=coul4, ylab="Dowry Death Cases", 
        main="Dowry Deaths V/S States/UT (2008)", las=2, cex.axis=0.8, cex.names=0.5,ylim = c(0,2500))


#For 2009
dowry_2009<-aggregate(x= crimes_2009$Dowry_Deaths,
                             by= list(crimes_2009$STATE.UT),
                             FUN=sum)
dowry_2009
states <- dowry_2009$Group.1
barplot(height = dowry_2009$x, name = states, col=coul4, ylab="Dowry Death Cases", 
        main="Dowry Deaths V/S States/UT (2009)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,2500))


#For 2010
dowry_2010<-aggregate(x= crimes_2010$Dowry_Deaths,
                             by= list(crimes_2010$STATE.UT),
                             FUN=sum)
dowry_2010
states <- dowry_2010$Group.1
barplot(height = dowry_2010$x, name = states, col=coul4, ylab="Dowry Death Cases", 
        main="Dowry Deaths V/S States/UT (2010)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,2500))


#For 2011
dowry_2011<-aggregate(x= crimes_2011$Dowry_Deaths,
                             by= list(crimes_2011$STATE.UT),
                             FUN=sum)
dowry_2011
states <- dowry_2011$Group.1
barplot(height = dowry_2011$x, name = states, col=coul4, ylab="Dowry Death Cases", 
        main="Dowry Deaths V/S States/UT (2011)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,2500))


#For 2012
dowry_2012<-aggregate(x= crimes_2012$Dowry_Deaths,
                             by= list(crimes_2012$STATE.UT),
                             FUN=sum)
dowry_2012
states <- dowry_2012$Group.1
barplot(height = dowry_2012$x, name = states, col=coul4, ylab="Dowry Death Cases", 
        main="Dowry Deaths V/S States/UT (2012)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,2500))

#Ques3 finished!! Every year Uttar Pradesh recorded the maximum numbers of Dowry Deaths.


#Ques4: What can you say about the crime rate of the following 5 states/UT: a)Delhi  b)Andhra Pradesh  c)West Bengal d)Madhya Pradesh e)Kerela     Did it increased or decreased over the period of time?

#State/UT 1: Delhi
dlcrimes <- crime_total[(crime_total$STATE.UT == "DELHI"),]
dlcrimes$total = rowSums(dlcrimes[,c(4,5,6,7,8,9,10)])
dlcrimes
plot(dlcrimes$Year, dlcrimes$total, type = "b", pch = 17, col = coul1, xlab = "Years", ylab = "Total no. of Crimes",main = "Crimes in Delhi V/S Years", ylim = c(2000,6000), )
#Ans: Increasing over the period of time.

#State/UT 2: Andhra Pradesh
apcrimes <- crime_total[(crime_total$STATE.UT == "ANDHRA PRADESH"),]
apcrimes$total = rowSums(apcrimes[,c(4,5,6,7,8,9,10)])
apcrimes
plot(apcrimes$Year, apcrimes$total, type = "b", pch = 17, col = coul1, xlab = "Years", ylab = "Total no. of Crimes",main = "Crimes in Andhra Pradesh V/S Years", ylim = c(10000,27000), )
#Ans: Increasing over the period of time.
#State/UT 3: West Bengal
wbcrimes <- crime_total[(crime_total$STATE.UT == "WEST BENGAL"),]
wbcrimes$total = rowSums(wbcrimes[,c(4,5,6,7,8,9,10)])
wbcrimes
plot(wbcrimes$Year, wbcrimes$total, type = "b", pch = 17, col = coul1, xlab = "Years", ylab = "Total no. of Crimes",main = "Crimes in West Bengal V/S Years", ylim = c(6000,31000), )

#State/UT 4: Madhya Pradesh
mpcrimes <- crime_total[(crime_total$STATE.UT == "MADHYA PRADESH"),]
mpcrimes$total = rowSums(mpcrimes[,c(4,5,6,7,8,9,10)])
mpcrimes
plot(mpcrimes$Year, mpcrimes$total, type = "b", pch = 17, col = coul1, xlab = "Years", ylab = "Total no. of Crimes",main = "Crimes in Madhya Pradesh V/S Years", ylim = c(14000,17000), )
#Ans: Till 2008 it was both increasing and decreasing simultaneously but the crime rate started increasing after 2008.

#State/UT 5: Kerala
klcrimes <- crime_total[(crime_total$STATE.UT == "KERALA"),]
klcrimes$total = rowSums(klcrimes[,c(4,5,6,7,8,9,10)])
klcrimes
plot(klcrimes$Year, klcrimes$total, type = "b", pch = 17, col = coul1, xlab = "Years", ylab = "Total no. of Crimes",main = "Crimes in Kerala V/S Years", ylim = c(5000,12000), )
#Ans: Increasing over the period of time.

#Ques 4 finished!!


#Ques5: Give a 5 number summary for the different crimes in Bihar for the year it had the maximum total no. of crimes!

crime_bihar<-crime_total[crime_total$STATE.UT=='BIHAR',] 
crime_total_bihar<-rowSums(crime_bihar[4:10])
print(crime_total_bihar)
print(max(crime_total_bihar))
#Bihar records highest crime in 2012

#so plotting box plot for 2012 crimes in bihar
crimes_bihar<-crimes_data[(crimes_data$STATE.UT=="BIHAR"),]

FilteredData_bihar = subset ( crimes_bihar,Year=='2012')

ggplot(FilteredData_bihar, aes(x = "Year", y = Rape, fill = Year)) + 
  stat_boxplot(geom='errorbar')+ 
  geom_boxplot() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 14)) +
  facet_wrap( ~ Year, scales="free")+
  theme(legend.position = "none")

ggplot(FilteredData_bihar, aes(x = "Year", y = Kidnapping_Abduction, fill = Year)) + 
  stat_boxplot(geom='errorbar')+ 
  geom_boxplot() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 14)) +
  facet_wrap( ~ Year, scales="free")+
  theme(legend.position = "none")

ggplot(FilteredData_bihar, aes(x = "Year", y = Dowry_Deaths, fill = Year)) + 
  stat_boxplot(geom='errorbar')+ 
  geom_boxplot() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 14)) +
  facet_wrap( ~ Year, scales="free")+
  theme(legend.position = "none")

ggplot(FilteredData_bihar, aes(x = "Year", y = Assaults, fill = Year)) + 
  stat_boxplot(geom='errorbar')+ 
  geom_boxplot() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 14)) +
  facet_wrap( ~ Year, scales="free")+
  theme(legend.position = "none")

ggplot(FilteredData_bihar, aes(x = "Year", y = Insults_to_modesty, fill = Year)) + 
  stat_boxplot(geom='errorbar')+ 
  geom_boxplot() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 14)) +
  facet_wrap( ~ Year, scales="free")+
  theme(legend.position = "none")

ggplot(FilteredData_bihar, aes(x = "Year", y = Cruelty_by_Husband, fill = Year)) + 
  stat_boxplot(geom='errorbar')+ 
  geom_boxplot() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 14)) +
  facet_wrap( ~ Year, scales="free")+
  theme(legend.position = "none")

ggplot(FilteredData_bihar, aes(x = "Year", y = Importation, fill = Year)) + 
  stat_boxplot(geom='errorbar')+ 
  geom_boxplot() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 14)) +
  facet_wrap( ~ Year, scales="free")+
  theme(legend.position = "none")

summary(FilteredData_bihar[,4:10])
# Interpretation
# The five number summary is a set of five values that denote:
# 1. Minimum: The minimum corresponds to the horizontal line
#   at the lower end of the bottom whisker in the boxplot.
# 2. First quartile: first quartile corresponds to the lower end of the box in the boxplot.
# 3. Second quartile (median): The median corresponds to the horizontal line in the box in the boxplot.
# 4. Third quartile: The third quartile corresponds to the upper end of the box in the boxplot.
# 5. Maximum:The maximum corresponds to the horizontal line at the upper end of the upper whisker in the boxplot


#Ques6: Compare the rates of Rapes and Kidnapping/Abduction in Rajasthan(district-wise) for the two years(take one year for which the total number of crimes was the maximum
# and other year for which the total number of crimes was minimum). 

tcrimes <- data.frame(Year = years, Total = totals)  
tcrimes
print(tcrimes[(tcrimes$Total==max(tcrimes$Total)),])   #232528 for 2012
print(tcrimes[(tcrimes$Total==min(tcrimes$Total)),])   #130725 for 2001

#for 2001(min no. of total crimes)
rajasthan_2001 <- crimes_2001[(crimes_2001$STATE.UT == "RAJASTHAN"),]
rajasthan_2001

#rapes in 2001
rapes_rj_2001 <- aggregate(x= rajasthan_2001$Rape,
                                    by= list(rajasthan_2001$DISTRICT),
                                    FUN=sum)
rapes_rj_2001   #district wise no. of rapes in rajasthan for 2001

#plotting the graph for rapes district-wise
barplot(height = rapes_rj_2001$x, name = rapes_rj_2001$Group.1, col=coul3, ylab="No. of Rapes", 
        main="Rape Cases V/S Districts of Rajasthan(2001)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,100))


#kidnapping/abduction in 2001
kidnapping_rj_2001 <- aggregate(x= rajasthan_2001$Kidnapping_Abduction,
                                by= list(rajasthan_2001$DISTRICT),
                                FUN=sum)   
kidnapping_rj_2001   #district wise no. of kidnapping/abduction in rajasthan for 2001

#plotting the graph for kidnapping district-wise
barplot(height = kidnapping_rj_2001$x, name = kidnapping_rj_2001$Group.1, col=coul3, ylab="No. of Kidnappings/Abductions", 
        main="Kidnapping/Abductions Cases V/S Districts of Rajasthan(2001)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,250))

#for 2012(max no. of total crimes)
rajasthan_2012 <- crimes_2012[(crimes_2012$STATE.UT == "RAJASTHAN"),]
rajasthan_2012

#rapes in 2012
rapes_rj_2012 <- aggregate(x= rajasthan_2012$Rape,
                           by= list(rajasthan_2012$DISTRICT),
                           FUN=sum)
rapes_rj_2012   #district wise no. of rapes in rajasthan for 2012

#plotting the graph for rapes district-wise
barplot(height = rapes_rj_2012$x, name = rapes_rj_2012$Group.1, col=coul3, ylab="No. of Rapes", 
        main="Rape Cases V/S Districts of Rajasthan(2012)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,150))


#kidnapping/abduction in 2012
kidnapping_rj_2012 <- aggregate(x= rajasthan_2012$Kidnapping_Abduction,
                                by= list(rajasthan_2012$DISTRICT),
                                FUN=sum)   
kidnapping_rj_2012   #district wise no. of kidnapping/abduction in rajasthan for 2012

#plotting the graph for kidnapping district-wise
barplot(height = kidnapping_rj_2012$x, name = kidnapping_rj_2012$Group.1, col=coul3, ylab="No. of Kidnappings/Abductions", 
        main="Kidnapping/Abductions Cases V/S Districts of Rajasthan(2012)", las=2, cex.axis=0.8, cex.names=0.5, ylim = c(0,200))
# Ques6 finished!!! We can compare the no. of rape cases and kidnapping/abduction cases by lookin at the graph simultaneously!!!



#Our questions end here!!
#Hence we conclude our project!!