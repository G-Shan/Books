
WHO = read.csv("WHO.csv")
str(WHO)

#plotting
plot(WHO$GNI, WHO$FertilityRate)


#Plotting the same variables using ggplot2. 
library(ggplot2)

#ggplot has 3 arguements. We mentioned 1st two in the Scatterplot variable. 3rd is added in next
Scatterplot = ggplot(WHO, aes(x=GNI, y=FertilityRate))
#Give the geometric properties as points
Scatterplot + geom_point()
#Give the geometric properties with a line
Scatterplot + geom_line()

#Replacing the points with small triangles
Scatterplot + geom_point(color="blue", size=3, shape=17)

#creating the graph with darkred stars 
Scatterplot + geom_point(color="darkred", size=3, shape=8)

#Giving a title to the graph
Scatterplot + geom_point(color="darkred", size=3, shape=8) + ggtitle("Fertility Rate vs. GNI")

#Save the plot to a file. 1st save it in a variab. then print it. then close the file by typing dev.off()
FertilityGNIPlot = Scatterplot + geom_point(color="darkred", size=3, shape=8)+ ggtitle("Fertility Rate vs. GNI")
pdf("MyPlot1.pdf")
print(FertilityGNIPlot)
dev.off()


#Let's color the points by REGIONS instead by adding a color option to our aesthetic,
ggplot(WHO, aes(x=GNI, y=FertilityRate, color=Region)) + geom_point()

#Color the points by LifeExpectancy now
ggplot(WHO, aes(x=GNI, y=FertilityRate, color=LifeExpectancy)) + geom_point()


#Plot of FertilityRate & Under15. Here there seems to be correlation but non-linear correlation
ggplot(WHO, aes(x=FertilityRate, y=Under15)) + geom_point()

ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point()

#Create a linear regression of Under15 & Log of FertilityRate
model = lm(Under15 ~ log(FertilityRate), data=WHO)
summary(model)

#Now lets add the regression line in our plot
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm")


#Above plot has 99% confidence interval. Lets make it to 95% in below plot
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm", level=0.99)

#We can also remove confidence interval by  typing se=FALSE. Now we've only Reg line in blue
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm", se=FALSE)

#CHanging the color of regression line to orange
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm", se=FALSE, color="orange")


#scale_color_brewer will give distinct color to all the Regions
ggplot(WHO, aes(x=log(FertilityRate), y=Under15, color=Region)) + geom_point() + scale_color_brewer(palette="Dark2")



##########THE ANALYTICAL POLICEMEN--VISUALIZATION OF LAW & ORDER###################

mvt = read.csv("mvt.csv", stringsAsFactors=TRUE)
str(mvt)

#We need to convert the Date in a format that R can understand using strptime function
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")
#Here 1st arg is the variable. 2nd argument is the formate of the variable

#Now that Date is variable, lets add day & hour in the data frame
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)

#No. of crimes on the weekdays
table(mvt$Weekday)

#Saving this crime data as a data frame to use in the ggplot
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts) #It has two variables

library(ggplot2)

#Here group=1 will group our data in one line since we want one line. Friday is shown 1st in the 
#plot since it showing days in alphabetical order.
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

#Lets see it in chronological order by making Var1 an ordered factor variable
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#NOw this is the plot we want. days are in chronological order
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

#Now lets us change x & y axis labels
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))+xlab("Day of the week")+ylab("Total Motor Vehicle Theft")


#Here linetype=2 means the line will be appear dashed
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1),linetype=2)+xlab("Day of the week")+ylab("Total Motor Vehicle Theft")

#Here alpha=0.3 makes the line lighter in color
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1),alpha=0.3)+xlab("Day of the week")+ylab("Total Motor Vehicle Theft")




####HEATMAP

#we want to make heatmap using day of the week and hour of the day
table(mvt$Weekday, mvt$Hour)

#Lets save this table to use in the ggplot
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts) #168 observations with 3 variables
#Here 2nd variable Var2 is factor variable. We need to convert it into numerical since it hour

#A factor Var is changed to numeric by changing 1st into character variable 1st
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))

#IN the plot there are 7 lines, one each for the day of the week but we cant say which line is whichday
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))

#THis will make the lines for days look in different colors. size=2 will make lines thicker
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1),size=2)

#Still the graph is not much interpretable. Lets make the heatmap now

#Before we make heatmap, we need to change the days to chronological order
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

#NOw lets make the heatmap. geom_tile makes the heatmap
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq))

#We can change the label on the legend,and get rid of the y label to make our plot a little nicer. element_black removes y axis labels
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Theft")+theme(axis.title.y=element_blank())

#We can also change the color scheme with scale_fill_gradient by giving high & low colors
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Theft", low="white", high="red")+theme(axis.title.y=element_blank())



#####GEOGRAPHICAL HEATMAP OF CHICAGO

#Install & load the packages
install.packages("maps")
install.packages("ggmap")
library(maps)
library(ggmap)

#Let's load the map of chicago
chicago = get_map(location="chicago", zoom=11)
ggmap(chicago)
Delhi = get_map(location="delhi", zoom=11)
ggmap(Delhi)

#let's plot the first 100 motor vehicle thefts in our data set on this map.
ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x=Longitude, y=Latitude))
#if we would hv plotted all 190k motor thefts, there wud be so many black dots and it would not make sense
#we're more interested in whether an area has high crime or not

#let's round our latitude and longitude to two digits of accuracy and create a crime counts data
#frame for each area.This gives crime counts at every grid
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))

str(LatLonCounts)
#Var1 is Longitude, Var2 is Latitude. Freq is no. of motor vehicle thefts

#Lets connvert var1 & var2 to numeric and give them proper names
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

#plot these points on our map,making the size and color of the points depend on the total number of motor vehicle thefts
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color=Freq, size=Freq))

#Let's change the color of this geograph by adding scale_color_gradient
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color=Freq, size=Freq)) + scale_color_gradient(low="yellow", high="red")

#We can also use geom_tile to make something that looks more like a traditional heat map.
ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x=Long, y=Lat, alpha=Freq), fill="red")

#Heatmap plotted some squares in the water as well. they're bcz Freq=0 is considered. we can remove them with Freq >0
LatLonCounts2 = subset(LatLonCounts, Freq > 0)
ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x=Long, y=Lat, alpha=Freq), fill="red")







##################MURDERS-- HEATMAP ON UNITED STATES###################################

murders = read.csv("murders.csv")
str(murders)

#Loading the packages for plots
library(ggplot2)
library(maps)
library(ggmap)

#Loading the coordinates for the map of USA
statesMap = map_data("state")
str(statesMap)

#Plotting the US. group variable defines how to draw the United States into groups by state.
ggplot(statesMap, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black")

#Since both murders & statesMap dataframe have states in different varibales, we need to match them.
#murders have states in the "State" variable with 1st lettter capital. statesMap has states in Region varibale
#with all lowercase.

#This will convert all the elements in State to lowercase and save in a new variable Region
murders$region = tolower(murders$State)

#Merge both the dataframes by the common identifier region. murderMap will hv all variables from both dataframes
murderMap = merge(statesMap, murders, by="region")
str(murderMap)

#let's plot the number of murders on our map of the United States. scale_fill makes plot color ranges from black to red. guide will give legends to plot
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Murders)) + geom_polygon(color="black")+scale_fill_gradient(low="black", high="red", guide="legend")
#As the plot shows, Calif & Texas have highest murder. but it might be bcz they're most populated, we dont know

#Color each state a/c to the population now by changing the fill variable
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Population)) + geom_polygon(color="black")+scale_fill_gradient(low="black", high="red", guide="legend")
#Since this plot looks similar to that of murders plot. we need to plot the murder rate

#Create a new variable murderRate which is no. of murders per 100,000 population
murderMap$MurderRate = murderMap$Murders/murderMap$Population*100000

#plot the graph using MurderRate
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=MurderRate)) + geom_polygon(color="black")+scale_fill_gradient(low="black", high="red", guide="legend")



#There aren't really any red states.Why? It turns out that Washington, DC is an outlier with a very high murder rate,but it's such a small region on the
#map that we can't even see it. So let's redo our plot, removing any observations with murder rates above 10, which we know will only exclude Washington, DC.
#Keep in mind that when interpreting and explaining the resulting plot, you should always note what you did to create it: removed Washington, DC from the data.

#Limit the graph for MurderRate upto 10. we know it will exclude only DC
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=MurderRate)) + geom_polygon(color="black")+scale_fill_gradient(low="black", high="red", guide="legend", limits=c(0,10))

#States with highest gun ownership
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=GunOwnership)) + geom_polygon(color="black")+scale_fill_gradient(low="black", high="red", guide="legend")






############RECITATIONS-- THE GOOD THE BAD THE UGLY VISUALIZATIONS#######################################

library(ggplot2)
intl = read.csv("intl.csv")
str(intl)

#Here geom_text will print the bars with their values. and stat = "identity" says, use the value of the y variable as is, which is what we want.
#The height of the bar is the value of the y variable. All the values of bars are b/w zero & one.
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity") + geom_text(aes(label=PercentOfIntl))
#As we can see here, histogram is in alphabetical order. we wanna chnage it a/c to the values


#Here we're ordering the intl a/c to the PercentOfIntl on decreasing order (by giving negative sign)
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)

#Lets multiply our PercentOfIntl value by 100
intl$PercentOfIntl = intl$PercentOfIntl*100

#Vjust=-0.4 it moves the labels lil bit up above bars. In theme, we've left the x title(xlab) blank and elements of x axis are rotated at 45 degree for better viewing
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity", fill="dark blue") + geom_text(aes(label=PercentOfIntl, vjust=-0.4))+ylab("percent of international students") + theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45,hjust=1))


##Lets download and plot intldata that on a world map instead and see if it is an effective way of communicating where students come from to MIT.
##So now we're going to try plotting a world map with a new data set that has the number of international students from each country.
library(gglpot2)
library(ggmap)
intlall = read.csv("intlall.csv", stringsAsFactors=FALSE)
str(intlall)
head(intlall)
#Here ExchangeVisiting is the data we need

#Here we're converting all the NAs to zeroes.
intlall[is.na(intlall)] = 0
head(intlall)

#Lets loads the world map now
world_map = map_data("world")
str(world_map)

#Merge the df worldmap & intlall by countries. region is for countries in worldmap and its citizenship in intlall
world_map = merge(world_map, intlall, by.x="region", by.y="Citizenship")

#Well, sometimes the merge can reorder the data. And it turns out that what the world_map data frame really is is actually a list of latitude and longitude points
#that define the border of each country. So if we accidentally reorder the data frame they no longer make any sense.And as it goes from point to point,
#the points might be on the other side of the country as it defines the polygon.
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black") + coord_map("mercator")

#THis plot doesnt make sense. We need to reorder the data

#Lets reorder the data in correct format. We're going to order the rows based on, first of all, the group, which is pretty much equivalent to the country,
#and then the order variable, which is just the correct order for the border points. And we're going to take all the columns, of course.
world_map = world_map[order(world_map$group, world_map$order),]

#Lets plot the world now. But notice that Russia is missing,and a lot of countries near it, as well as China.Which is definitely not true because I
#have many friends at MIT who are from Russia and China.The reason China is missing is that it has a different name in the MIT data frame
#than in the world_map data frame.So when we merged them, it was dropped from the data set because it didn't match up.
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black") + coord_map("mercator")

#In intlall, CHina is called "China (People's Republic)" while it's only China in world_map
table(intlall$Citizenship)

#Lets rename China appropriately
intlall$Citizenship [intlall$Citizenship=="China (People's Republic Of)"] = "China"
table(intlall$Citizenship)

#Lets remerge the data once again after fixing China
world_map = merge(map_data("world"), intlall, by.x="region", by.y="Citizenship")
#REordering the data
world_map = world_map[order(world_map$group, world_map$order),]

#Lets plot the map now. This plot is mercator projection
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Total), color="black") + coord_map("mercator")

#Lets plot the same map in 3D projection now
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(aes(fill=Total), color="black") + coord_map("ortho", orientation=c(20,30,0))




##############RECITATION----US HOUSEHOLDS##########################

library(ggplot2)
households = read.csv("households.csv")
str(households)
#We're not sure what to use in aesthetic function. The reason is that ggplot needs it in the form of: year, group, and fraction. The solution is to use the melt function from
#the reshape package.Melt will take a 2-dimensional data frame like ours,and convert it into exactly the right form we need for ggplot2.

library(reshape2)

#all rows and 1st two columns of households dataframe
households[,1:2]

#Now, let's look at the first few rows of our melted households data frame.
#So, basically, what's happened is that each value of MarriedWChild has turned into its own row in the new data frame.
head(melt(households, id="Year"))

#We're taking "Values" & Variable from melt function
ggplot(melt(households, id="Year"), aes(x=Year, y=value, color=variable)) + geom_line(size=2) + geom_point(size=5) + ylab("Percentage of Households")
