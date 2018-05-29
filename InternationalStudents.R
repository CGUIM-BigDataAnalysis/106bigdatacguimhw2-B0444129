#1
library(readr)
c103 <- read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=25f64d5125016dcd6aed42e50c972ed0")
s103<- read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=a6d1469f39fe41fb81dbfc373aef3331")
c104 <- read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=4d3e9b37b7b0fd3aa18a388cdbc77996")
s104<- read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=8baeae81cba74f35cf0bb1333d3d99f5")
c105 <- read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=19bedf88cf46999da12513de755c33c6")
s105<- read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=1a485383cf9995da679c3798ab4fd681")
c106 <- read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=50e3370f9f8794f2054c0c82a2ed8c91")
s106<- read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=883e2ab4d5357f70bea9ac44a47d05cc")



library(dplyr)
library(knitr)
c103<-data.frame(country=c103$國別,people103=c103$`學位生-正式修讀學位外國生`+c103$`學位生-僑生(含港澳)`+c103$`學位生-正式修讀學位陸生`+
                   c103$`非學位生-外國交換生`+c103$`非學位生-外國短期研習及個人選讀`+
                   c103$`非學位生-大專附設華語文中心學生`+c103$`非學位生-大陸研修生`+
                   c103$`非學位生-海青班`+c103$境外專班)

c104<-data.frame(country=c104$國別,people104=c104$`學位生-正式修讀學位外國生`+c104$`學位生-僑生(含港澳)`+c104$`學位生-正式修讀學位陸生`+
                   c104$`非學位生-外國交換生`+c104$`非學位生-外國短期研習及個人選讀`+
                   c104$`非學位生-大專附設華語文中心學生`+c104$`非學位生-大陸研修生`+
                   c104$`非學位生-海青班`+c104$境外專班)
c103104<-full_join(c103,c104,by="country")

c105<-data.frame(country=c105$國別,people105=c105$`學位生_正式修讀學位外國生`+c105$`學位生_僑生(含港澳)`+c105$`學位生_正式修讀學位陸生`+
                   c105$`非學位生_外國交換生`+c105$`非學位生_外國短期研習及個人選讀`+
                   c105$`非學位生_大專附設華語文中心學生`+c105$`非學位生_大陸研修生`+
                   c105$`非學位生_海青班`+c105$境外專班)
c103104105<-full_join(c103104,c105,by="country")

c106<-data.frame(country=c106$國別,people106=c106$`學位生_正式修讀學位外國生`+c106$`學位生_僑生(含港澳)`+c106$`學位生_正式修讀學位陸生`+
                   c106$`非學位生_外國交換生`+c106$`非學位生_外國短期研習及個人選讀`+
                   c106$`非學位生_大專附設華語文中心學生`+c106$`非學位生_大陸研修生`+
                   c106$`非學位生_海青班`+c106$境外專班)
c103104105106<-full_join(c103104105,c106,by="country")
c103104105106[is.na(c103104105106)]=0
c103104105106$total<-c103104105106$people103+c103104105106$people104+c103104105106$people105+c103104105106$people106
c103104105106<-arrange(c103104105106,desc(c103104105106$total))
knitr::kable(c103104105106[1:10,c(1,6)])


s103$`非學位生-大陸研修生`<-as.numeric(gsub("…","0",s103$`非學位生-大陸研修生`))
s103<-data.frame(school=s103$學校名稱,people103=s103$`學位生-正式修讀學位外國生`+s103$`學位生-僑生(含港澳)`+s103$`學位生-正式修讀學位陸生`+
                   s103$`非學位生-外國交換生`+s103$`非學位生-外國短期研習及個人選讀`+s103$`非學位生-大專附設華語文中心學生`+
                   s103$`非學位生-大陸研修生`+s103$`非學位生-海青班`+s103$境外專班)

s104$`非學位生-大陸研修生`<-as.numeric(gsub("…","0",s104$`非學位生-大陸研修生`))
s104<-data.frame(school=s104$學校名稱,people104=s104$`學位生-正式修讀學位外國生`+s104$`學位生-僑生(含港澳)`+s104$`學位生-正式修讀學位陸生`+
                   s104$`非學位生-外國交換生`+s104$`非學位生-外國短期研習及個人選讀`+s104$`非學位生-大專附設華語文中心學生`+
                   s104$`非學位生-大陸研修生`+s104$`非學位生-海青班`+s104$境外專班)
s103104<-full_join(s103,s104,by="school")

s105$`非學位生_大陸研修生`<-as.numeric(gsub("…","0",s105$`非學位生_大陸研修生`))
s105<-data.frame(school=s105$學校名稱,people105=s105$`學位生_正式修讀學位外國生`+s105$`學位生_僑生(含港澳)`+s105$`學位生_正式修讀學位陸生`+
                   s105$`非學位生_外國交換生`+s105$`非學位生_外國短期研習及個人選讀`+s105$`非學位生_大專附設華語文中心學生`+
                   s105$`非學位生_大陸研修生`+s105$`非學位生_海青班`+s105$境外專班)
s103104105<-full_join(s103104,s105,by="school")

s106$`非學位生_大陸研修生`<-as.numeric(gsub("…","0",s106$`非學位生_大陸研修生`))
s106<-data.frame(school=s106$學校名稱,people106=s106$`學位生_正式修讀學位外國生`+s106$`學位生_僑生(含港澳)`+s106$`學位生_正式修讀學位陸生`+
                   s106$`非學位生_外國交換生`+s106$`非學位生_外國短期研習及個人選讀`+s106$`非學位生_大專附設華語文中心學生`+
                   s106$`非學位生_大陸研修生`+s106$`非學位生_海青班`+s106$境外專班)
s103104105106<-full_join(s103104105,s106,by="school")
s103104105106[is.na(s103104105106)]=0
s103104105106$total<-s103104105106$people103+s103104105106$people104+s103104105106$people105+s103104105106$people106
s103104105106<-arrange(s103104105106,desc(s103104105106$total))
knitr::kable(s103104105106[1:10,c(1,6)])


#2
library(ggplot2)
ggplot()+geom_bar(data=c103104105106,
                  aes(x=country,y=total),
                  stat = "identity") +labs(x="國家",y="人數")+theme(axis.text.x = element_text(angle = 90, hjust = 0.001))
groupCountry1<-c103104105106%>%
  group_by(country)%>%
  tally(total,sort=TRUE)%>%
  group_by(country = factor(c(country[1:10], rep("Other", n() - 10)),
                                     levels = c(country[1:10], "Other")))%>%
  tally(n)
colnames(groupCountry1)<-c("country","total")
ggplot()+geom_bar(data=groupCountry1,
                  aes(x=country,y=total),
                  stat = "identity",
                  fill = "red")

#3
library(readr)
Countriesname <- read_csv("C:/Users/CHIUCHUNWEI/Desktop/CountriesComparisionTable.csv")
colnames(Countriesname)<-c("ISO3","English","country")
TotalCountry<-merge(c103104105106[,c(1,6)],Countriesname,by="country")
colnames(TotalCountry)<-c("country","value","ISO3","region")
TotalCountry[5,2]<-TotalCountry[5,2]+TotalCountry[91,2]+TotalCountry[159,2]
TotalCountry[107,2]<-TotalCountry[107,2]+TotalCountry[108,2]
TotalCountry<-TotalCountry%>%
  subset(region!="Unmatch")%>%
  subset(country!="索馬利蘭共和國")
a3<-country_choropleth(TotalCountry)
a3

#4
library(readr)
Student_RPT_07 <- read_csv("Student_RPT_07.csv")
library(dplyr)
a<-subset(Student_RPT_07,`學年度`>102)%>%
group_by(`對方學校(機構)國別(地區)`)%>%
summarise(nCountry=sum(小計))
b<-arrange(a,desc(nCountry))
knitr::kable(b[1:10,])

c<-subset(Student_RPT_07,`學年度`>102)%>%
group_by(`學校名稱`)%>%
  summarise(nSchool=sum(小計))
d<-arrange(c,desc(nSchool))
knitr::kable(d[1:10,])

#5
library(ggplot2)
ggplot()+geom_bar(data=a,
                  aes(x=`對方學校(機構)國別(地區)`,y=nCountry),
                  stat = "identity") +labs(x="國家",y="人數")+theme(axis.text.x = element_text(angle = 90, hjust = 0.001))
groupCountry2<-a%>%
  group_by(`對方學校(機構)國別(地區)`)%>%
  tally(nCountry,sort=TRUE)%>%
  group_by(`對方學校(機構)國別(地區)` = factor(c(`對方學校(機構)國別(地區)`[1:10], rep("Other", n() - 10)),
                                     levels = c(`對方學校(機構)國別(地區)`[1:10], "Other")))%>%
  tally(n)
colnames(groupCountry2)<-c("對方學校(機構)國別(地區)","nCountry")
ggplot()+geom_bar(data=groupCountry2,
                  aes(x=`對方學校(機構)國別(地區)`,y=nCountry),
                  stat = "identity",
                  fill = "red")

#6
library(readr)
countryname <- read_csv("C:/Users/CHIUCHUNWEI/Desktop/CountriesComparisionTable.csv")
colnames(countryname)<-c("ISO3","English","中文")
counta<-a
colnames(counta)<-c("國名","count")
countb<-merge(counta,countryname,by.x="國名",by.y="中文")
colnames(countb)<-c("國名","value","ISO3","region")
countb<-countb%>%
  subset(region!="Unmatch")
a6<-country_choropleth(countb,num_colors=9)
a6

#7
library(dplyr)
library(readr)
m105 <- read_csv("C:/Users/CHIUCHUNWEI/Downloads/m105.csv")
m105$X4=NULL
m105$X5=NULL
m105$X6=NULL
m105[is.na(m105)]=0
m105<-arrange(m105,desc(m105$總人數))
knitr::kable(m105[1:10,])

#8
library(readr)
countryname <- read_csv("C:/Users/CHIUCHUNWEI/Desktop/CountriesComparisionTable.csv")
colnames(countryname)<-c("ISO3","English","國別")
m105countryname<-merge(m105,countryname,by="國別")
colnames(m105countryname)<-c("國名","洲別","value","ISO3","region")
a8<-country_choropleth(m105countryname)
a8

#9
