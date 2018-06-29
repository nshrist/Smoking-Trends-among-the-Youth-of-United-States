# Smoking-Trends-among-the-Youth-of-United-States

R CODE FOR ANALYSIS AND VISUALS:
•	Data Cleaning
setwd("C:/Users/Shristiraj Nishant/OneDrive/BI Project")
getwd()
data1<- read.csv("R Proj Data.csv", header = T, sep = ",")

project<- data.frame(data1)

View(project)

install.packages("tidyr")
library(tidyr)
project_clean1<- project[, -c(4,7,9,10,20:23,28)]
View(project_clean1)
project_clean1<- data.frame(project_clean1)

clean2<- separate(project_clean1, Race.Age.Education, c("Race", "Age", "Education"), sep = "-" )

View(clean2)
clean2$Data_Value[is.na(clean2$Data_Value)]= mean(clean2$Data_Value, na.rm = T)

clean2$Data_Value_Std_Err[is.na(clean2$Data_Value_Std_Err)]= mean(clean2$Data_Value_Std_Err, na.rm = T)

clean2$Data_Value_Std_Err[is.na(clean2$Data_Value_Std_Err)]= mean(clean2$Data_Value_Std_Err, na.rm = T)

clean2$High_Confidence_Limit[is.na(clean2$High_Confidence_Limit)]= mean(clean2$High_Confidence_Limit, na.rm = T)

clean2$Sample_Size[is.na(clean2$Sample_Size)]= mean(clean2$Sample_Size, na.rm = T)

View(clean2)
clean3<- data.frame(clean2)
View(clean3)
•	Barplot for Top 5 States in terms of Cessation Rate
install.packages(dplyr)

library(dplyr)
df3<- select(clean3, YEAR, LocationDesc, TopicDesc, Measure.Description, Data_Value)

View(df3)
df4<- filter(df3, TopicDesc == 'Cessation (Youth)')

df4<- data.frame(df4)

View(df4)

df5<- arrange(df4, desc(Data_Value))

View(df5)

df6<- distinct(df5, LocationDesc, TopicDesc, Data_Value)

View(df6)

df7<- head(df6, n= 5)

View(df7)

df6<- distinct(df5, LocationAbbr,LocationDesc, TopicDesc, Data_Value)

df7<- head(df6, n= 5)

View(df7)

c<- c("yellow","turquoise","violetred3","dodgerblue", "goldenrod")

x<- barplot(df7$Data_Value, names.arg = df7$LocationAbbr, xlab = "States", ylab = "Cessation rate", main = "Top 5 States in Terms of Cessation Rate in USA", col = c, ylim = c(0,100))

text(x,5,df7$Data_Value)







•	Pie-Chart for Generation Wise Current Cigarette Smokers
View(clean3)
df8<- subset(clean3, Response == 'Current')
View(df8)

df8<- subset(clean3, Response == 'Current' & TopicDesc == 'Cigarette Use (Youth)' & StratificationID1 == '1GEN')

df9<- subset(clean3, Response == 'Current' & TopicDesc == 'Cigarette Use (Youth)' & StratificationID1 == '2GEN')
df10<- subset(clean3, Response == 'Current' & TopicDesc == 'Cigarette Use (Youth)' & StratificationID1 == '3GEN')
m1<- func(df8)
m2<- func(df9)
m2<- func(df10)
v< - data.frame(m1,m2,m3)
slices<- round(v/sum(v)*100,1)
lab<- paste(slices, "%", sep = " ")
pie(v, labels = lab, main = "Generation Wise Current Cigarette Smokers in USA", col = rainbow(length(v)))
legend("topright", c("1 GENERATION", "2 GENERATION", "3 GENERATION"), cex = 0.8, fill = rainbow(length(v) ))
# User Defined Function: func()
func<- function(N)
{
a <-round((N$Data_Value*N$Sample_Size)/100)
b<- sum(a)
return(b)
}
•	R- Script for Line chart to find number of Male and Female Smokers who tried to quit smoking
library(dplyr)
mf<- select(clean3, YEAR, LocationAbbr,LocationDesc, TopicDesc, Measure.Description, Data_Value, Gender)
View(mf)
mf1<- filter(mf, TopicDesc == 'Cessation (Youth)' & Gender == 'Male')
View(mf1)
mf2<- filter(mf, TopicDesc == 'Cessation (Youth)' & Gender == 'Female')
View(mf2)
mf1<- data.frame(mf1)
mf2<- data.frame(mf2)
mf3<- filter(mf1, YEAR==2010)
View(mf3)
 mf4<- filter(mf1, YEAR==2011)
 mf5<- filter(mf1, YEAR==2012)
 mf6<- filter(mf1, YEAR==2013)
 mf7<- filter(mf1, YEAR==2014)
 mf8<- filter(mf1, YEAR==2015)
 rel_males<- c(mean(mf3$Data_Value), mean(mf4$Data_Value), mean(mf5$Data_Value),    mean(mf6$Data_Value),mean(mf7$Data_Value), mean(mf8$Data_Value))
  rel_males
  mf9<- filter(mf2, YEAR==2010)
  mf10<- filter(mf2, YEAR==2011)
  mf11<- filter(mf2, YEAR==2012)
  mf12<- filter(mf2, YEAR==2013)
  mf13<- filter(mf2, YEAR==2014)
  mf14<- filter(mf2, YEAR==2015)
  rel_females<- c(mean(mf9$Data_Value), mean(mf10$Data_Value), mean(mf11$Data_Value), mean(mf12$Data_Value),mean(mf13$Data_Value), mean(mf14$Data_Value))
   rel_females
   height_y<- range(0,rel_males, rel_females)
   labs<- c("2010", "2011", "2012","2013","2014","2015")
   plot(rel_males, type = "o", col= "blue", ylim = height_y, axes = F, ann = F)
   axis(1, at=1:6, labels = labs)
   axis(2, las=1, at=10*0:height_y[2])
   lines(rel_females, type="o", pch=22, lty=2, col="black")
   title(main="Number of Male vs Female Smokers", col.main="Black", font.main=4, xlab = "Years", ylab = "Pecentage of Quit Attempts")
   box()
   legend("right", c("Males","Females"), cex=0.8, col=c("blue”, “black"), pch=21:22, lty=1:2)
Execution of script:
source(“program.R”)
