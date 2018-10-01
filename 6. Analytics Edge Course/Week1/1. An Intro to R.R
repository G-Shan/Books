
Country = c("Brazil", "China", "India","Switzerland","USA")
LifeExpectancy = c(74,76,65,83,79)

# Data Frames
CountryData = data.frame(Country, LifeExpectancy) 
# two vectors with diff modes are combined using dataframe

CountryData$Population = c(199000,1390000,1240000,7997,318000) 
#new variable can be added in the data frame using dollar sign

Country = c("Australia","Greece")
LifeExpectancy = c(82,81)
Population = c(23050,11125)
NewCountryData = data.frame(Country, LifeExpectancy, Population)

AllCountryData = rbind(CountryData, NewCountryData)
#New observations can be added in the data frame using rbind function

###############WHO.CSV##############

WHO = read.csv("WHO.CSV")
str(WHO)
mean(WHO$Over60)

which.max(WHO$LiteracyRate) #Highest literacy rate. it lies in the 44th row
WHO$Country[44]  #country of the 44th row. It has highest literacy rate

which.min(WHO$Over60)
WHO$Country[183]

WHO_Europe = subset(WHO, Region == "Europe")
#Make a new dataframe only for EU countries

write.csv(WHO_Europe, "WHO_Europe.CSV")
#Save the data frame WHo Europe into a CSV file WHO_Europe
summary(WHO$Over60)

plot(WHO$GNI, WHO$FertilityRate) #Drawing a plot

Outliars = subset(WHO, GNI > 10000 & FertilityRate > 2.5) #Creating a subset with two conditions

nrow(Outliars) #Checking the rows of Outliars
#7 countries which have higher GNI (> 10k) & fertitlity rate (> 2.5)

Outliars[c("Country", "GNI", "FertilityRate")]

hist(WHO$CellularSubscribers)

boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab="", ylab="Life Expectancy", main="Life expectancy of countries by region")

table(WHO$Region)

tapply(WHO$Over60, WHO$Region, mean) 
#tapply will take mean of the Over60 and bifurcate on the basis of region


tapply(WHO$LiteracyRate, WHO$Region, mean, na.rm=T) #Remove all the NA values




#######################RECITATION--FOOD###################################################################


USDA = read.csv("USDA.CSV")
str(USDA)
USDA$Sodium

which.max(USDA$Sodium) #Wich element has maxm sodium. it is 265th row

names(USDA) #we dont know which column to look for maxm sodium in 65th row

USDA$Description[265] # we need to use Descrip since it has all the elements

HighSodium = subset(USDA, Sodium > 10000)
#create a new fn highsodium which has over 10k sodium content

nrow(HighSodium) #rows in the HighSodium

HighSodium$Description #names of top 10 sodium foods


############Amount of Sodium in Caviar##############
match("CAVIAR", USDA$Description) #find the index of Caviar in Descrip column
USDA$Sodium[4154] #sodium level in caviar is 1500

#Finding sodium lvl in caviar took us two steps. We can do it in one step as well
USDA$Sodium[match("CAVIAR", USDA$Description)]
########################################################################


summary(USDA$Sodium)
sd(USDA$Sodium, na.rm=T) #Find std deviation. NA values hv been removed


############Creating Plots in the R i.e. Visualization################

plot(USDA$Protein, USDA$TotalFat, xlab="Protein", ylab="Fat", main="Protien Vs. Fat", col="red")


#Histogram takes only one value since y-axis is always frequency
hist(USDA$VitaminC, xlab="Vitamin C (mg)", main="Histogram of VitaminC levels")
#Chart doesnt make sense since most of our food (over 6k) has less than 200 mg of vitamin c

hist(USDA$VitaminC, xlab="Vitamin C (mg)", main="Histogram of VitaminC levels", xlim = c(0,100))
#Here we've limited value of x-axis from 0-100 only

hist(USDA$VitaminC, xlab="Vitamin C (mg)", main="Histogram of VitaminC levels", xlim=c(0,100), breaks=100)
#Here we set the cells to 100. but it will show only 5 cells each in 20 mg of length. Vitaminc has max value of 2000
#Here R has divided 2000 with 100 and gave us 5 cells each of lenth 20. But still we want 100 cells each of 1mg.
#So we need to devide 2000 by 2000. here xlim is 100. so we'll hv 100 cellls

hist(USDA$VitaminC, xlab="Vitamin C (mg)", main="Histogram of VitaminC levels", xlim=c(0,100), breaks=2000)
#Here over 5000 of food has less than 1mg of vitamin c


boxplot(USDA$Sugar, main = "Boxplot of Sugar level", ylab = "Sugar (g)")



############ADDING VARIABLES##########################
USDA = read.csv("USDA.CSV")

USDA$Sodium[1] > mean(USDA$Sodium, na.rm=T)
#checking if the 1st of food in sodium column has higher sodium than the average
USDA$Sodium[50] > mean(USDA$Sodium, na.rm=T)

GoodSodium = (USDA$Sodium > mean(USDA$Sodium, na.rm=T))
#Creating a new variable HighSodium

str(GoodSodium) #It's a logical/boolean variable


#but we want HighSod in 0,1 form instead of T & F values
GoodSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=T))
str(GoodSodium)

#Now we wanna add GoodSodium in the USDA data set
USDA$GoodSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=T))
str(USDA)

USDA$GoodProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=T))
USDA$GoodFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=T))
USDA$GoodCarb = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=T))
str(USDA)

table(USDA$GoodSodium) #2090 foods have higher than average sodium
table(USDA$GoodSodium, USDA$GoodFat)

#Show the average amount of iron levels in foods sorted by high & low level of protein
tapply(USDA$Iron, USDA$GoodProtein, mean, na.rm=T) #foods with low protein hv on avg 2.55 mg of iron


tapply(USDA$Iron, USDA$GoodProtein, summary, na.rm=T)
