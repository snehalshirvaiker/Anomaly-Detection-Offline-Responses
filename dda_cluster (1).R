install.packages("klaR")
library(klaR)
a<-kmodes(data = data[,3:22],modes = 8)
a$cluster

data<- cbind(data, a$cluster)
View(data)
a$withindiff
a$weighted
a$iterations

b<-dist(data[,c(-2,-23,-24)],method = "euclidean")
View(b)

install.packages("StatMatch")
library(StatMatch)

install.packages("gower")
library(gower)

library(cluster)

library(readxl)
data1<- read_excel("C:/Users/sneha/Downloads/data entry (1).xlsx", 
                            sheet = "Into 1 2 ")
View(data1)

data1[,3:22]<- as.factor(data[,3:22])
class(data1)
str(data1)


data1$`Question 1`<-as.factor(data1$`Question 1`)
data1$`Question 2`<-as.factor(data1$`Question 2`)
data1$`Question 3`<-as.factor(data1$`Question 3`)
data1$`Question 4`<-as.factor(data1$`Question 4`)
data1$`Question 5`<-as.factor(data1$`Question 5`)
data1$`Question 6`<-as.factor(data1$`Question 6`)
data1$`Question 7`<-as.factor(data1$`Question 7`)
data1$`Question 8`<-as.factor(data1$`Question 8`)
data1$`Question 9`<-as.factor(data1$`Question 9`)
data1$`Question 10`<-as.factor(data1$`Question 10`)
data1$`Question 11`<-as.factor(data1$`Question 11`)
data1$`Question 12`<-as.factor(data1$`Question 12`)
data1$`Question 13`<-as.factor(data1$`Question 13`)
data1$`Question 14`<-as.factor(data1$`Question 14`)
data1$`Question 15`<-as.factor(data1$`Question 15`)
data1$`Question 16`<-as.factor(data1$`Question 16`)
data1$`Question 17`<-as.factor(data1$`Question 17`)
data1$`Question 18`<-as.factor(data1$`Question 18`)
data1$`Question 19`<-as.factor(data1$`Question 19`)
data1$`Question 20`<-as.factor(data1$`Question 20`)
str(data1)

a<-kmodes(data = data1[,3:22],modes = 8)
a$cluster
a
plot(jitter(data1[,3:22]),col=a$cluster)


c<- daisy(x = data1[,3:22],metric = "gower")
c
cc<- as.matrix(c)
ccd<- as.data.frame(cc)


View(cc)
min(cc)
max(cc)
cc[cc>0&cc<0.1]
which(cc>0&cc<0.1)
1221/50

cca<- as.dist(ccd)
h<-hclust(cca,method = "ward.D")
plot(h)



