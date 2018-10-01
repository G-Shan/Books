#############ASSIGNMENT 1 MVT#############################

mvt = read.csv("mvtWeek1.csv")
str(mvt)
max(mvt$ID) #maximum value of ID
min(mvt$Beat)
table(mvt$Arrest) #For how many thefts, arrest (TRUE) was made

summary(mvt) #How many locationsDescrip have Alley
mvt$Date[1]
mvt$Date[500]

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M")) #Convert the date character
summary(DateConvert) #Median of Date in month and year

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)

mvt$Date = DateConvert

table(mvt$Arrest, mvt$Month) #Fewest arrest in which month
table(mvt$Weekday) #most theft on which day

hist(mvt$Date, breaks=100)
boxplot(mvt$Date ~ mvt$Arrest)

table(mvt$Arrest, mvt$Year) #proportion of theft in 2007 in which arrest was made

sort(table(mvt$LocationDescription)) #Top locations where most of the theft happened


top1 = subset(mvt, LocationDescription == "STREET")
top2 = subset(mvt, LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)")
top3 = subset(mvt, LocationDescription == "ALLEY")
top4 = subset(mvt, LocationDescription == "GAS STATION")
top5 = subset(mvt, LocationDescription == "DRIVEWAY - RESIDENTIAL")

Top6 = rbind(top1,top2,top3,top4,top5)
nrow(Top6)
str(Top6)

Top6$LocationDescription = factor(Top6$LocationDescription)

table(Top6$Arrest, Top6$LocationDescription) #Highest propotion of arrest in location?


table(top4$Weekday) #most theft at gas station on which day
table(top5$Weekday) #most theft at Driveway on which day
##Or we can use the below formula for above problems
table(Top6$LocationDescription, Top6$Weekday)





###########ASSIGNMENT 2 STOCK DYNAMICS#######################################
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")
str(IBM)


#Changing the date format so that R could read it
#Here 1st argument(IBM$Date) is the variable we wanna change. 2nd argument (%m/%d/%y) is the format of date
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
summary(IBM)
summary(GE)
summary(ProcterGamble)
summary(CocaCola)
summary(Boeing)

sd(ProcterGamble$StockPrice) #std deviation of PnG

################VISUALIZATIONS############################

plot(CocaCola$Date, CocaCola$StockPrice, xlab="Date", ylab="Stock Price", type="l", col="red") #Here l means line

#We can add PnG graph in the plot using "lines" function
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")


#This will create a vertical line on 1st March 2000. lwd=2 means thick line
abline(v=as.Date(c("2000-03-01")), lwd=2)


#Here we wanna consider prices from 1995 to 2005. Index of 1995 is 301 and 432 for 2005.
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="green")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="orange")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="black")

abline(v=as.Date(c("2000-03-01")), lwd=2)
#it will create a vertical line lwd 2 means thick line.



#Here comparing the avg monthly prices of IBM with overall mean prices
tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)





######ASSIGNMENT-2 DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES##############

CPS = read.csv("CPSData.csv")
str(CPS)
summary(CPS)

table(CPS$Industry) #Highest no. people employed in which kind of industry

sort(table(CPS$State)) #States with highest and lowest interviewies
table(CPS$Citizenship)

table(CPS$Race, CPS$Hispanic) #How many interviewis belong to Hispanic as well as other races

table(CPS$Region, is.na(CPS$Married)) #people bifurcated on region whether have Married value as missing (NA)
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))


table(CPS$Region, is.na(CPS$MetroAreaCode)) 
#People who live in metropolitan area (TRUE) and non-metropolitan areas (FALSE)

table(CPS$State, is.na(CPS$MetroAreaCode)) 
str(CPS$State)

sort(tapply(is.na(CPS$MetroAreaCode),CPS$State, mean))
#Which state has a proportion of interviewees living in a non-metropolitan area closest to 30% (Wisconsin)
#Which state has the largest proportion of non-metropolitan interviewees, ignoring states where all 
#interviewees were non-metropolitan (it's Montana)


########MERGING THE DATA####
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")
str(MetroAreaMap)
str(CountryMap)



CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
#The first two arguments determine the data frames to be merged (they are called 
#"x" and "y", respectively, in the subsequent parameters to the merge function). 
#by.x="MetroAreaCode" means we're matching on the MetroAreaCode variable from the "x" 
#data frame (CPS), while by.y="Code" means we're matching on the Code variable from the 
#"y" data frame (MetroAreaMap). Finally, all.x=TRUE means we want to keep all rows from 
#the "x" data frame (CPS), even if some of the rows' MetroAreaCode doesn't match any codes
#in MetroAreaMap 


summary(CPS)
names(CPS)
sort(table(CPS$MetroArea)) #Which Metropolitan area has highest interviewies

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
#WHich metropolitan area has highest proportion of Hispanic interveiwes


sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))
#How many metropolitan areas have over 20% Asian interviewis


sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=T))
# Bifurcating Interviewis on the basis of Metro area who have no dilpoma. missing values removed


#Merging the CountrMap with CPS
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
str(CPS)
sort(summary(CPS$Country)) #Among all interviewees born outside of North America, which 
#country was the most common place of birth (Phillipines)


sort(tapply(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country, mean, na.rm=T))

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE)) #Which metropolitan area has the largest 
#number (note -- not proportion) of interviewees with a country of birth in India


