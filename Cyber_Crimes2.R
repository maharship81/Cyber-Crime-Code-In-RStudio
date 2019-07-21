crimes_2017 <- read.csv("crimes_2017.csv", header = TRUE)
crimes_2018_19 <- read.csv("crimes_2018-19.csv", header = TRUE)

combi <- rbind(crimes_2017,crimes_2018_19)

dim(crimes_2017)
dim(crimes_2018_19)

head(crimes_2017)
head(crimes_2017,5)
tail(crimes_2017)
tail(crimes_2017,5)
library(psych)
headTail(combi$IUCR)

mean(combi$CommunityArea)
median(combi$CommunityArea)
range(combi$CommunityArea)
quantile(combi$District)
quantile(combi$District,c(0.25, 0.75))
length(combi$CommunityArea[combi$CommunityArea <= 20]) / length(combi$CommunityArea) * 100
var(combi$District)
sd(combi$District)
min(combi$Beat)
max(combi$Beat)

str(crimes_2017)

table(is.na(crimes_2017))

table(combi$Year)

colSums(is.na(crimes_2017))

sort(combi$CommunityArea)

sum(combi$CommunityArea)

nrow(crimes_2017)

ncol(crimes_2017)

ls(crimes_2017)

describe(crimes_2017)

describe(combi$CommunityArea)

summary(crimes_2017)

class(crimes_2017$Date)

DateConvert = as.Date(strptime(crimes_2017$Date, "%m/%d/%y %H:%M"))
crimes_2017$Month = months(DateConvert)
crimes_2017$Weekday = weekdays(DateConvert)
crimes_2017$Date = DateConvert
class(crimes_2017$Date)

hist(crimes_2017$Wards, breaks=100)

library(MASS)      
tbl = table(crimes_2017$Arrest, crimes_2017$District) 
tbl  
chisq.test(tbl)

tbl1 = table(crimes_2017$District, crimes_2017$Arrest) 
tbl1  
chisq.test(tbl1)

b1=sqldf("SELECT count(*) FROM crimes_2017 WHERE Ward BETWEEN 1 AND 30")
b1
b2=sqldf("SELECT count(*) FROM crimes_2017 WHERE Ward BETWEEN 40 AND 60")
b2
b3=sqldf("select count(*) From crimes_2017 WHERE Ward BETWEEN 45 AND 65")
b3

b50=c(b1$`count(*)`,b2$`count(*)`,b3$`count(*)`)
b50

barplot(b50,names.arg = "Beat", main = "Year",ylim = c(-10,5000),col=rainbow(10),las=2)
ans=sqldf('select count(Year)"Total_Year",Year from crimes_2017 group by IUCR')
ans

c5=sqldf('select Location Description,count(Year) from crimes_2017 group by Arrest')
c5

q20=sqldf('select count(Beat) from crimes_2017 where Ward="8"')
q20
ans5=sqldf('select count(Beat) from crimes_2017 where Ward="12"')
ans5

cl = c("red","blue")
reg=c("8","12")

reg1=c(q20$`count(Beat)`,ans5$`count(Beat)`)
reg1

barplot(reg1,names.arg =reg,col=cl,xlab="Ward",ylab = "Number of Year",ylim = c(-10,200))

c5=sqldf('select Description,count(Beat) from crimes_2017 group by Description')
c5

ggplot(combi, aes(x = combi$`FBICode`)) +
  geom_bar() + coord_flip() + xlab("FBICode") + ylab("Number of Respondents")

e <- data.frame(f=rnorm(1000))
g <- ggplot(data=e, aes(x=f))
g <- g + geom_histogram(bins = 30)
g

no_cyl <- data.frame(table(combi$Year))
barplot(table(combi$Year),col=cl)

g <- ggplot(no_cyl, aes(x=Var1, y=Freq, fill = Var1))
g <- g + geom_bar(stat = "identity")
g <- g  + labs(x="No. of Cylinders", ylab="Frequency", title="Bar Plot")
g

boxplot(combi$Beat)

counts <- table(combi$Wards)
barplot(counts, main="Wards Distribution",  xlab="Number of Wards",ylab = "Wards")

ans=combi$Beat
hist(ans)

library(leaflet)
m = leaflet()
m = addTiles(m)
m = addMarkers(m, lat=41.65470, lng=-87.61051, popup="pc")
m

library(dplyr)
combi$Year[is.na(combi$Year)] <- median(combi$Year, na.rm = TRUE)
table(is.na(combi$Year))

plot(x = combi$CommunityArea, y = combi$District, type = 'p')

coef(lm(Beat ~ CommunityArea, data = crimes_2017))

library(psych)               
describe(combi$Beat,type=3)

c1=c5$Description
c2=c5$`count(Beat)`
barplot(c2,names.arg = c1,col=rainbow(length(c1)),las=2,xlab="City",ylab = "number of Beat")

a1=sqldf('select Beat>1000,count(Description) from crimes_2017 where Description = "SIMPLE"')
a1
a2=sqldf('select Beat>1000,count(Description) from crimes_2017 where Description = "MANU/DELIVER:CRACK"')
a2
a3=sqldf('select Beat>1000,count(Description) from crimes_2017 where Description = "ARMED: HANDGUN"')
a3
reg=c("SIMPLE","MANU/DELIVER:CRACK","	ARMED: HANDGUN")
reg3=c(a1$`count(Description)`,a2$`count(Description)`,a3$`count(Description)`)
barplot(reg3,names.arg = reg,col=rainbow(length(reg)),ylim = c(-10,700))

res1 <- sqldf("select 100*sum(Beat=='NULL')/count(Beat) as 'NULL1',100*sum(Beat!='NULL')/count(Beat) as 'NOTNULL' from crimes_2017;")
res2=res1$NULL1
res3=res1$NOTNULL
res4=rbind(res2,res3)
co=c("black","Red")
pie(res4,col=co,main="Null Vlaues from Description")
pielabels=c("98.68% - Null","1.32% - Not Null")
legend("bottomright",legend=pielabels,bty="n",fill=co,cex=0.5)

res1 <- sqldf("select 100*sum(Ward=='NULL')/count(Ward) as 'NULL1',100*sum(Ward!='NULL')/count(Ward) as 'NOTNULL' from crimes_2017;")
res2=res1$NULL1
res3=res1$NOTNULL
res4=rbind(res2,res3)
co=c("black","maroon")
pie(res4,col=co,main="Null Vlaues")
pielabels=c("23.50% - Null","76.50% - Not Null")
legend("bottomright",legend=pielabels,bty="n",fill=co,cex=0.5)

Description_m=sqldf("select count(*) from crimes_2017 where LocationDescription='STREET' AND PrimaryType='BATTERY'")
Description_m
Description_f=sqldf("select count(*) from crimes_2017 where LocationDescription='STREET' AND PrimaryType='CRIMINAL DAMAGE'")
Description_f
Description_m1=sqldf("select count(*) from crimes_2017 where LocationDescription='RESIDENCE' AND PrimaryType='BATTERY'")
Description_m1
Description_f1=sqldf("select count(*) from crimes_2017 where LocationDescription='RESIDENCE' AND PrimaryType='CRIMINAL DAMAGE'")
Description_f1
reg=c("BATTERYSTREET","CRIMINAL DAMAGESTREET","BATTERYRESIDENCE","CRIMINAL DAMAGERESIDENCE")
reg1_m_f=c(Description_m$`count(*)`,Description_f$`count(*)`,Description_m1$`count(*)`,Description_f1$`count(*)`)
barplot(reg1_m_f,names.arg = reg,main = "PrimaryType wise LocationDescription",xlab="PrimaryType",ylab="LocationDescription",ylim = c(-10,300),col=rainbow(length(reg1_m_f)),las=2,cex.names = 0.8)

q10=sqldf("select District,min(Wards) from crimes_2017 where District between 10 and 20 group by District")
q10
District=q10$District

reg_District=(q10$`min(Wards)`)
reg_District
barplot(reg_District,names.arg =District,main = "Minimum Weight of year 1960 to 2016",ylim = c(0,30),ylab = "Height",xlab = "Year",col = rainbow(length(District)),las=1)

x=crimes_2017$Wards
x=hist(x,main = "Wards between 10 to 55",xlab = "Wards",ylab = "Wards related frquency",col = rainbow(length(x)),xlim =c(1,70),ylim = c(1,1000))

x=crimes_2017$CommunityArea
x=hist(x,main = "CommunityArea between 10 to 80",xlab = "Wards",ylab = "CommunityArea related frquency",col = rainbow(length(x)),xlim =c(1,80),ylim = c(1,1000))

g=sqldf('select * from crimes_2017 order by Wards');
x=g$ZipCode
x1=tail(x,7)
a=g$FBICode
a1=tail(a,7)
s=g$Beat
s1=tail(s,7)
a1
s1
data=c(120,23,56,98,55,66,44,120,200,300,21,36,52,32)
m=matrix(data,nrow=2,ncol=7,byrow = TRUE)
barplot(m,names.arg=x1,las=2,col=rainbow(2),ylim = c(0,500))
l=c('FBICode','Beat');
legend("topleft",l,fil=rainbow(2),cex=0.3)

ans=sqldf("select count(Beat),District from crimes_2017 group by District")
y=ans$`count(Beat)`
x=ans$District
barplot(y,names.arg=x,col=rainbow(length(x)),las=2,ylim = c(0,500))

g=sqldf("select * from crimes_2017 order by District ")
c=g$Beat
c1=tail(c,7)
c1
a=g$District
District=tail(a,7)
pie(District,c1,col=rainbow(length(c1)))

p=sqldf("select Arrest,count(Arrest) from crimes_2017 group by Arrest")
p
ans=p$`count(Arrest)`
barplot(ans,names.arg =p$Arrest,col=c("cyan","magenta"),ylim = c(0,6000),ylab = "Height",xlab = "Arrest")