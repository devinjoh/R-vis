# Assignment 9 question 2



# I got the data from the FRED economic research site.  https://research.stlouisfed.org/  http://www.utah-demographics.com/counties_by_population
# it was imported as an excel file.  I did some rearranging/formatting of the data in excel, but it was mostly clean.  

# load packages 
library(lubridate)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(grid)

#upload Data
utah <- as.data.frame(read.csv("/Users/devinjohanson/Documents/BYU/Winter 2017/Data Visualization/Utah_County_Unemployment 2.csv"))

utahtp <- as.data.frame(read.csv("/Users/devinjohanson/Documents/BYU/Winter 2017/Data Visualization/unemployment utah transposed.csv"))

utahpop<- as.data.frame(read.csv("/Users/devinjohanson/Documents/BYU/Winter 2017/Data Visualization/population utah counties.csv"))


utahpop$col <- ifelse(utahpop$Population>=100000,"#d7301f",ifelse(utahpop$Population>=50000,"#fc8d59",ifelse(utahpop$Population>=20000,"#fdcc8a","#fef0d9" )))


#Clean Data 
my.colors <- brewer.pal(7,"OrRd")

utahtp$County <- as.character(utahtp$County)
utahtp$Unemployment <-as.numeric(utahtp$Unemployment)

utahtp$DATE <-as.character(utahtp$DATE)
#utah$DATE <-as.Date(utah$DATE, "%m%/%d%/%y%")
utah$DATE <-as.Date(mdy(utah$DATE))
#utahtp$DATE <-as.Date(utahtp$DATE, "%m%/%d%/%y%")
utahtp$DATE <-as.Date(mdy(utahtp$DATE))


#generate plots
a <-xyplot(utahtp$Unemployment~utahtp$DATE,group=utahtp$County, type="l",xlab= " ", ylab="Unemployment Rate (Unadjusted for Seasons)", col=utahpop$col, 
           main="Unemployment in Utah by County: 1990-2016", par.settings=list(par.main.text=list(cex=3))
           ,key=list(corner=c(1,1),
                     lines=list(col=c("#d7301f","#fc8d59","#fdcc8a","#fef0d9"), lty=5, lwd=10), title="County Population",
                     text=list(c("100,000 - 1,000,000", "50,000 - 100,000", "20,000 - 50,000", "Less than 20,000")))
           ,scales = list(alternating = c(1,0), tck =c(1,0))
           ,page = function(page) grid.text('Source:FRED https://research.stlouisfed.org/', x = 0.5, y = 0.02))

b <-xyplot(utah$Salt.Lake.County~ utah$DATE, type="l", lwd=2, col="#d7301f")
c <-xyplot(utah$Utah.County ~ utah$DATE, type="l",lwd=2, col="#d7301f")

d <-xyplot(utah$Tooele.County~ utah$DATE, type="l", lwd=1.5, col="#fc8d59")
e <-xyplot(utah$Box.Elder.County ~ utah$DATE, type="l",lwd=1.5, col="#fc8d59")
a + as.layer(b+c+e+d)



########## Second Portion of Visualziation with maps ############

#load packages
library(maps)
library(mapdata)


 #import data
data(county.fips)
#I did some of the cleanup and combination work in excel instead.  Look at my csv files to see how.  
utahfips <- as.data.frame(read.csv("/Users/devinjohanson/Documents/BYU/Winter 2017/Data Visualization/utah county fips.csv"))
my.colors <- brewer.pal(7,"OrRd")
my.colors
#march 1990



#clean data
utah1990 <-subset(utahtp,utahtp$DATE=="1990-03-01",c(1:3))

utah1990m <-merge.data.frame(utah1990,utahfips, by.x="County", by.y="County")

utah1990m$value = utah1990m$Unemployment
utah1990m$region = utah1990m$Fips

utah1990m$col <- ifelse(utah1990m$value>=12,"#990000",ifelse(utah1990m$value>=10, "#D7301F",ifelse(utah1990m$value>=8,"#EF6548", ifelse(utah1990m$value>=6,"#FC8D59",ifelse(utah1990m$value>=4,"#FDBB84",ifelse(utah1990m$value>=3,"#FDD49E","#FEF0D9"))))))


#Generate Maps 

#maps 1990
map("county", "utah", regions=utah1990m$name, col=utah1990m$col, exact=FALSE, fill=TRUE, resolution=0, lty=0, projection="mercator")
map("county","utah", fill=FALSE, col="black", add=TRUE, lty=1, lwd=1, projection="mercator")
mtext("1990", side=1, cex = 3.5)
legend("left", c("0-3%", "3-4%", "4-6%", "6-8%", "8-10%","10-12%", "<12%"), col =  c("#FEF0D9", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#990000"), fill=my.colors, bty="n")

#march 2000

utah2000 <-subset(utahtp,utahtp$DATE=="2000-03-01",c(1:3))

utah2000m <-merge.data.frame(utah2000,utahfips, by.x="County", by.y="County")

utah2000m$value = utah2000m$Unemployment
utah2000m$region = utah2000m$Fips

utah2000m$col <- ifelse(utah2000m$value>=12,"#990000",ifelse(utah2000m$value>=10, "#D7301F",ifelse(utah2000m$value>=8,"#EF6548", ifelse(utah2000m$value>=6,"#FC8D59",ifelse(utah2000m$value>=4,"#FDBB84",ifelse(utah2000m$value>=3,"#FDD49E","#FEF0D9"))))))

map("county", "utah", regions=utah2000m$name, col=utah2000m$col, exact=FALSE, fill=TRUE, resolution=0, lty=0, projection="mercator")
map("county","utah", fill=FALSE, col="black", add=TRUE, lty=1, lwd=1, projection="mercator")
mtext("2000", side=1, cex = 3.5)

#march 2004

utah2004 <-subset(utahtp,utahtp$DATE=="2004-03-01",c(1:3))

utah2004m <-merge.data.frame(utah2004,utahfips, by.x="County", by.y="County")

utah2004m$value = utah2004m$Unemployment
utah2004m$region = utah2004m$Fips

utah2004m$col <- ifelse(utah2004m$value>=12,"#990000",ifelse(utah2004m$value>=10, "#D7301F",ifelse(utah2004m$value>=8,"#EF6548", ifelse(utah2004m$value>=6,"#FC8D59",ifelse(utah2004m$value>=4,"#FDBB84",ifelse(utah2004m$value>=3,"#FDD49E","#FEF0D9"))))))

map("county", "utah", regions=utah2004m$name, col=utah2004m$col, exact=FALSE, fill=TRUE, resolution=0, lty=0, projection="mercator")
map("county","utah", fill=FALSE, col="black", add=TRUE, lty=1, lwd=1, projection="mercator")
mtext("2004", side=1, cex = 3.5)


#2007
utah2007 <-subset(utahtp,utahtp$DATE=="2007-03-01",c(1:3))

utah2007m <-merge.data.frame(utah2007,utahfips, by.x="County", by.y="County")

utah2007m$value = utah2007m$Unemployment
utah2007m$region = utah2007m$Fips

utah2007m$col <- ifelse(utah2007m$value>=12,"#990000",ifelse(utah2007m$value>=10, "#D7301F",ifelse(utah2007m$value>=8,"#EF6548", ifelse(utah2007m$value>=6,"#FC8D59",ifelse(utah2007m$value>=4,"#FDBB84",ifelse(utah2007m$value>=3,"#FDD49E","#FEF0D9"))))))

map("county", "utah", regions=utah2007m$name, col=utah2007m$col, exact=FALSE, fill=TRUE, resolution=0, lty=0, projection="mercator")
map("county","utah", fill=FALSE, col="black", add=TRUE, lty=1, lwd=1, projection="mercator")
mtext("2007", side=1, cex = 3.5)


#march 2010
utah2010 <-subset(utahtp,utahtp$DATE=="2010-03-01",c(1:3))

utah2010m <-merge.data.frame(utah2010,utahfips, by.x="County", by.y="County")

utah2010m$value = utah2010m$Unemployment
utah2010m$region = utah2010m$Fips

utah2010m$col <- ifelse(utah2010m$value>=12,"#990000",ifelse(utah2010m$value>=10, "#D7301F",ifelse(utah2010m$value>=8,"#EF6548", ifelse(utah2010m$value>=6,"#FC8D59",ifelse(utah2010m$value>=4,"#FDBB84",ifelse(utah2010m$value>=3,"#FDD49E","#FEF0D9"))))))

map("county", "utah", regions=utah2010m$name, col=utah2010m$col, exact=FALSE, fill=TRUE, resolution=0, lty=0, projection="mercator")
map("county","utah", fill=FALSE, col="black", add=TRUE, lty=1, lwd=1, projection="mercator")
mtext("2010", side=1, cex = 3.5)

#march 2016
utah2016 <-subset(utahtp,utahtp$DATE=="2016-03-01",c(1:3))

utah2016m <-merge.data.frame(utah2016,utahfips, by.x="County", by.y="County")

utah2016m$value = utah2016m$Unemployment
utah2016m$region = utah2016m$Fips

utah2016m$col <- ifelse(utah2016m$value>=12,"#990000",ifelse(utah2016m$value>=10, "#D7301F",ifelse(utah2016m$value>=8,"#EF6548", ifelse(utah2016m$value>=6,"#FC8D59",ifelse(utah2016m$value>=4,"#FDBB84",ifelse(utah2016m$value>=3,"#FDD49E","#FEF0D9"))))))

map("county", "utah", regions=utah2016m$name, col=utah2016m$col, exact=FALSE, fill=TRUE, resolution=0, lty=0, projection="mercator")
map("county","utah", fill=FALSE, col="black", add=TRUE, lty=1, lwd=1, projection="mercator")
mtext("2016", side=1, cex = 3.5)




# Combine Graphs
par(mfrow=c(1,6))

map("county", "utah", regions=utah1990m$name, col=utah1990m$col, exact=FALSE, fill=TRUE, resolution=0, lty=0, projection="mercator")
map("county","utah", fill=FALSE, col="black", add=TRUE, lty=1, lwd=1, projection="mercator")
mtext("1990", side=1, cex = 2)
legend("left", c("0-3%", "3-4%", "4-6%", "6-8%", "8-10%","10-12%", "<12%"), col =  c("#FEF0D9", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#990000"), fill=my.colors, bty="n")

map("county", "utah", regions=utah2000m$name, col=utah2000m$col, exact=FALSE, fill=TRUE, resolution=0, lty=0, projection="mercator")
map("county","utah", fill=FALSE, col="black", add=TRUE, lty=1, lwd=1, projection="mercator")
mtext("2000", side=1, cex = 2)

map("county", "utah", regions=utah2004m$name, col=utah2004m$col, exact=FALSE, fill=TRUE, resolution=0, lty=0, projection="mercator")
map("county","utah", fill=FALSE, col="black", add=TRUE, lty=1, lwd=1, projection="mercator")
mtext("2004", side=1, cex = 2)

map("county", "utah", regions=utah2007m$name, col=utah2007m$col, exact=FALSE, fill=TRUE, resolution=0, lty=0, projection="mercator")
map("county","utah", fill=FALSE, col="black", add=TRUE, lty=1, lwd=1, projection="mercator")
mtext("2007", side=1, cex = 2)

map("county", "utah", regions=utah2010m$name, col=utah2010m$col, exact=FALSE, fill=TRUE, resolution=0, lty=0, projection="mercator")
map("county","utah", fill=FALSE, col="black", add=TRUE, lty=1, lwd=1, projection="mercator")
mtext("2010", side=1, cex = 2)

map("county", "utah", regions=utah2016m$name, col=utah2016m$col, exact=FALSE, fill=TRUE, resolution=0, lty=0, projection="mercator")
map("county","utah", fill=FALSE, col="black", add=TRUE, lty=1, lwd=1, projection="mercator")
mtext("2016", side=1, cex = 2)



# I exported all the graphs and changed the overall formatting using adobe illustrator.  See final image for the output.  


