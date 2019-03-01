# Read CSV into R
globalorder <- read.csv(file="D:/Github/Global Order Analysis/Global Superstore 2016 Orders.csv", header=TRUE, sep = ',')
#convert year as numeric
globalorder$Order.Year<-as.numeric(as.character(globalorder$Order.Year))
#create a data frame for total sales of each market
library(dplyr)
order_count <- as.data.frame(globalorder %>% group_by(Market) %>% summarise(Total_Sale=sum(Sales)))
#View(order_count)

#Create a horizontal barplot of Total Sales by Market, ordered descending on total sales 
library(ggplot2)

#convert the data frame to ascending order
order_count<-order_count[order(order_count$Total_Sale,decreasing = FALSE),]

#switch to horizontal bars
barplot(order_count$Total_Sale, 
        names.arg = order_count$Market,
        xlab = "Total Sales",
        col = c("light grey","light grey","light grey","light grey","light grey","light grey","light grey"),
        horiz = TRUE,
        xlim = range(0,max(order_count$Total_Sale)))
title("Total Sales by Market",adj = 0)

#visually indicate the market which appear to be an outlier
barplot(order_count$Total_Sale, 
        names.arg = order_count$Market,
        xlab = "Total Sales",
        las=1,
        space = 0.75,
        col = c("dark blue","light grey","light grey","light grey","light grey","light grey","light grey"),
        horiz = TRUE,
        xlim = range(0,max(order_count$Total_Sale)))
title("Total Sales by Market",adj = 0)

#plot line graph for showing total slaes per year for each market
globalorder <- read.csv(file="D:/NEU Courses/Communication and Visualization/Week2/Week2_assignment/Global Superstore 2016 Orders.csv", header=TRUE, sep = ',')

#create a data frame for total sales of each market per year


library(dplyr)
year_sale <- as.data.frame(globalorder %>% group_by(Market,Order.Year) %>% summarise(Total_Sale=sum(Sales)),Year=Order.Year)
#year_sale$Order.Year<-as.numeric(as.character(year_sale$Order.Year))
#View(year_sale)
#create plot without X-axis
plot(as.numeric(year_sale$Order.Year),year_sale$Total_Sale,
     xlab = "Year", 
     ylab = "",
     xaxt="n",
     las=1)
     #main = "Total Sales per year of each market")
#customize the X-axis
axis(1,labels =  as.character(year_sale$Order.Year),
     at=as.numeric(year_sale$Order.Year))
#give title name
title("Total Sales per year for each market",adj = 0)

#make a data frame for each market and include the year and total sale
year_sale.Africa<-year_sale[year_sale$Market=="Africa",]
year_sale.APAC<-year_sale[year_sale$Market=="APAC",]
year_sale.Canada<-year_sale[year_sale$Market=="Canada",]
year_sale.EMEA<-year_sale[year_sale$Market=="EMEA",]
year_sale.EU<-year_sale[year_sale$Market=="EU",]
year_sale.LATAM<-year_sale[year_sale$Market=="LATAM",]
year_sale.US<-year_sale[year_sale$Market=="US",]

#find x and y axis range
xrange<-range(year_sale$Order.Year)
yrange<-range(year_sale$Total_Sale)

#plot the range first
plot(xrange, 
     yrange, 
     ylim = range(0,max(year_sale$Total_Sale)),
     las=1,
     xlab="Year",
     ylab="",
     xaxt="n")
title("Total Sales per year for each market",adj = 0)

#modify axes
axis(1, labels = as.character(year_sale$Order.Year), at = as.numeric(year_sale$Order.Year))

#add point markers
points(year_sale.Africa$Order.Year,year_sale.Africa$Total_Sale,type = "p",pch=1)
points(year_sale.APAC$Order.Year,year_sale.APAC$Total_Sale,pch=1,type = "p")
points(year_sale.Canada$Order.Year,year_sale.Canada$Total_Sale,pch=1,type = "p")
points(year_sale.EMEA$Order.Year,year_sale.EMEA$Total_Sale,pch=1, type="p")
points(year_sale.EU$Order.Year,year_sale.EU$Total_Sale,pch=1,type= "p")
points(year_sale.LATAM$Order.Year,year_sale.LATAM$Total_Sale,pch=1,type = "p")
points(year_sale.US$Order.Year,year_sale.US$Total_Sale,pch=1,type = "p")

#add lines
lines(year_sale.Africa$Order.Year,year_sale.Africa$Total_Sale,col = "Red",pch=18,type = "b")
lines(year_sale.APAC$Order.Year,year_sale.APAC$Total_Sale, col = "Dark orange",pch=18,type = "b")
lines(year_sale.Canada$Order.Year,year_sale.Canada$Total_Sale, col= "Dark Blue",pch=18,type = "b")
lines(year_sale.EMEA$Order.Year,year_sale.EMEA$Total_Sale, col="Green",pch=18,type = "b")
lines(year_sale.EU$Order.Year,year_sale.EU$Total_Sale, col = "Hot Pink",pch=18,type="b")
lines(year_sale.LATAM$Order.Year,year_sale.LATAM$Total_Sale, col = "Black",pch=18,type="b")
lines(year_sale.US$Order.Year,year_sale.US$Total_Sale, col="Purple",type = "b", pch=18)

#add text to highlight each market
text(2013,1195000,"APAC",cex = 1.0,adj = -2)
text(2013,1025000,"EU",cex = 1.0,adj = -5)
text(2013,775000,"US",cex = 1.0,adj = -6)
text(2013,635000,"LATAM",cex = 1.0,adj = -2)
text(2013,330000,"EMEA",cex = 1.0,adj = -2.5)
text(2013,245000,"Africa",cex = 1.0,adj = -3)
text(2013,75000,"Canada",cex = 1.0,adj = -2)

#add a legend
#legend(2011,1200000,legend = c("Africa","APAC","Canada","EMEA","EU","LATAM","US"),
 #      col=c("Red","Blue","Green","Medium Violet Red","Hot Pink","Black","Purple"),lty = 1:2,cex = 0.8)

#indicate outlier in line graph
#add line to indicate the outlier
lines(year_sale.Africa$Order.Year,year_sale.Africa$Total_Sale,col = "light grey",pch=18,type = "b")
lines(year_sale.APAC$Order.Year,year_sale.APAC$Total_Sale, col = "light grey",pch=18,type = "b")
lines(year_sale.Canada$Order.Year,year_sale.Canada$Total_Sale, col= "Dark Blue",pch=18,type = "b",lwd=4)
lines(year_sale.EMEA$Order.Year,year_sale.EMEA$Total_Sale, col="light grey",pch=18,type = "b")
lines(year_sale.EU$Order.Year,year_sale.EU$Total_Sale, col = "light grey",pch=18,type="b")
lines(year_sale.LATAM$Order.Year,year_sale.LATAM$Total_Sale, col = "light grey",pch=18,type="b")
lines(year_sale.US$Order.Year,year_sale.US$Total_Sale, col="light grey",type = "b", pch=18)

#add the text to highlight the market having the lowest sales
text(2013,75000,"Canada",cex = 1.2,adj = -2)

#create a box plot
#attach databse to R search path so that objects in the database cab be accessed simply using their names
attach(globalorder)
#to compare the sales of different market, separate the sales with the market
boxplot(Sales~Market,
        ylab="Sales",
        las=1,
        ylim=range(0,1200))
title("Comparison of the Sales of different markets", adj=0)

#Highlight the canadian market
boxplot(Sales~Market,
        ylab="Sales",
        las=1,
        ylim=range(0,1200),
        col= c("Light Grey","Light Grey", "Dark Blue","Light Grey","Light Grey","Light Grey","Light Grey"))
title("Comparison of the Sales of different markets", adj=0)

#scatter plot to show relation between Sales and Profit for various categories
#create new data frames for each category having profit and sales
myvars<-names(globalorder) %in% c("Category", "Sales", "Profit")
Tech_sale<-globalorder[which(Category=='Technology'),]
Tech_sale<-Tech_sale[myvars]

off_sale<-globalorder[which(Category=='Office Supplies'),]
off_sale<-off_sale[myvars]

fur_sale<-globalorder[which(Category=='Furniture'),]
fur_sale<-fur_sale[myvars]

#plot the range 
profitrange<-range(min(globalorder$Profit),globalorder$Profit)
salerange<-range(0,globalorder$Sales)
plot(salerange, 
     profitrange, 
     ylim = range(min(globalorder$Profit),max(globalorder$Profit)),
     xlim = range(0,max(globalorder$Sales)),
     las=1,
     xlab="Selling price of a product",
     ylab="Profit earned",
     col="White")
title("Profit Vs Selling Cost for the products of different Categories", adj=0)

#draw different point of each category
points(fur_sale$Sales,fur_sale$Profit,type = "p",pch=17,col="Dark Grey")
points(off_sale$Sales,off_sale$Profit,type = "p",pch=15,col="Blue")
points(Tech_sale$Sales,Tech_sale$Profit,type = "p",pch=16,col="Red")
#draw regression line for technology
regpoint_tech<-lm(Tech_sale$Profit ~ Tech_sale$Sales)
abline(regpoint_tech, col="Red",lwd=5)

#draw regression line for Furniture
regpoint_fur<-lm(fur_sale$Profit ~ fur_sale$Sales)
abline(regpoint_fur,col="Dark Grey")

#draw regression line for Office Supplies
regpoint_off<-lm(off_sale$Profit ~ off_sale$Sales)
abline(regpoint_off,col="Blue", lwd=1)

#Add legend
legend("topleft", 
       legend = c("Technology", "Furniture","Office Supplies"), 
       col = c("Red","Dark Grey","Blue"), 
       pch = c(16,17,15), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black",
       title = "Categories")



