library(tidyverse)
library(dplyr)
library(ggplot2)


heart <- read.csv("heart.csv", sep=",")
heart

data <- na.omit(heart)
data

data %>%
  select(age,sex,cp,trestbps,chol,fbs,restecg,thalach,exang,oldpeak,slope,ca,thal,target) %>%
  mutate(disease = as.logical(target)) %>%
  ggplot() + geom_boxplot(aes(group=disease,y=age,fill=disease))

  
summary(data$age)

#age
p<-ggplot(data, aes(x=age)) + 
  geom_histogram(color="black",bins = 10, fill="white")

install.packages("ggplot2")     # Install ggplot2 package
library("ggplot2")      p + geom_vline(aes(xintercept=mean(age)),
               color="blue", linetype="dashed", size=1)

#thalach
summary(data$thalach)

p<-ggplot(data, aes(x=thalach)) + 
  geom_histogram(color="black",bins = 1, fill="white")

p + geom_vline(aes(xintercept=mean(thalach)),
               color="blue", linetype="dashed", size=1)
data %>%
  group_by(sex) %>%
  summarise(n=n()) %>%
  mutate(Sex = as.logical(sex)) %>%
  ggplot() + geom_col(aes(x=Sex,y=n,fill=Sex)) %>%
  crosstab(Survey, row.vars = "age", col.vars = "sex", type = "f")

ggplot(data) +
  geom_bar(aes(x=cp,=cp))



ggplot(data) + 
  geom_histogram(aes(x=age),color="black",bins = 30, fill="white") + geom_line(data)



ggplot(data, aes(x = age, y = ..density..)) + geom_histogram(bins = 15, fill = "blue", color = "white", size = 0.2) + 
  geom_line(data = data, aes(x = age, y = ..density..), color = "red", size = 1)



data %>%
  group_by(age) %>%
  summarise(n=n()) %>%
  ggplot(data,aes(x=age,y=n)) + geom_histogram(color="black",bins = 30, fill="white") + geom_line(data)

summary(data$age)
hist(data$age, freq = FALSE,breaks=seq(from=29,to=77,by=2)) + lines(density(data$age))

hist(data$thalach, freq = FALSE) + lines((data$thalach))


pie(age,labels)


heatmap(data$age, Colv = NA, Rowv = NA, scale="column")

heatmap(as.matrix(data$age))


ggplot(data,aes(x=age)) +
  geom_histogram(color="black",bins = 30, fill="white") + geom_line(data)

  
ggplot(df, aes(x = sex , y = value, fill = group)) + 
  geom_bar(width = 1, stat = 'identity') + coord_polar("y", start = 0)
print(p)




ggplot(data) +
  geom_density2d(aes(x=age,y=thalach),color="lightblue3",size=1.2)


ggplot(data[data$disease != "no disease", ])+
  geom_histogram(aes(x=thalach),fill="lightblue3",bins=15)






data <- read.csv("heart.csv")
summary(data)
is.na(data)
str(data)
anyNA(data, recursive = FALSE)
library(caret)
library(tidyverse)
library(psych)


hist(data$age)

hist(data$age, 
     main="Histogram of Age", 
     xlab="Age", 
     border="darkslateblue", 
     col="darkslategray1")

boxplot(data$age,
        main="Boxpkot of Age", 
        ylab="Age", 
        border="darkslateblue", 
        col="darkslategray1")

y<-ifelse(data$target == 0,"no disease","disease")
data$disease = y
data$disease

age_nodisease<-ggplot(data[data$disease != "disease", ], aes(x=age)) + 
  geom_histogram(color="black",bins = 10, fill="darkslategray1")

age_nodisease + geom_vline(aes(xintercept=mean(age)),
                           color="deeppink", linetype="dashed", size=1)


age_disease<-ggplot(data[data$disease != "no disease", ], aes(x=age)) + 
  geom_histogram(color="black",bins = 10, fill="darkslategray1")

age_disease + geom_vline(aes(xintercept=mean(age)),color="deeppink", linetype="dashed", size=1)


by(data$age, data$disease, summary)

describe(data$age)

ggplot(data)+
  geom_bar(aes(x=disease,fill= disease))

pie_disease = ggplot(data, aes(x="", y=disease, fill=disease)) + geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) 

pie_disease

y<-ifelse(data$sex == 0,"Female","Male")
data$sex_char = y

ggplot(data)+
  geom_bar(aes(x=sex_char,fill=sex_char)) +   
  labs( title="Heart Disease Value",
        x ="Disease",
        y ="Amount",
  )

pie_sex = ggplot(data, aes(x="", y=sex_char, fill=sex_char)) + geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0)
pie_sex

table(data$sex_char,data$disease)

ggplot(data)+
  geom_bar(aes(x=disease,fill=sex_char),position = 'dodge')+
  labs( title="Heart Disease Frequency for Sex",
        x ="Disease",
        y ="Amount"
  )

table(data$cp)

ggplot(data) +
  geom_bar(aes(x=cp,fill=as.character(cp)))+
  labs( title="Chest pain type vs count")

table(data$sex_char,data$cp)

ggplot(data)+
  geom_bar(aes(x=sex_char,fill=as.character(cp)),position = 'dodge')+
  labs( title="Type of chest pain for sex")

table(data$disease,data$cp)

ggplot(data)+
  geom_bar(aes(x=cp,fill=disease),position = 'dodge')

ggplot(data)+
  geom_histogram(aes(x=age),fill="lightblue3")
hist(data$age, freq = FALSE) + lines(density(data$age))

ggplot(data)+
  geom_histogram(aes(x=thalach),fill="lightblue3")
hist(data$thalach, freq = FALSE) + lines(density(data$thalach))

ggplot(data) +
  geom_point(aes(x=age,y=thalach,color=disease),position = "jitter")

ggplot(data) +
  geom_density2d(aes(x=age,y=thalach),color="lightblue3",size=1.2)

ggplot(data[data$disease != "no disease", ])+
  geom_histogram(aes(x=thalach),fill="lightblue3",bins=15)

ggplot(data[data$disease != "disease", ])+
  geom_histogram(aes(x=thalach),fill="lightblue3",bins=15)

ggplot(data)+
  geom_histogram(aes(x=chol),fill="lightblue3",bins=15)

ggplot(data) +
  geom_point(aes(x=age,y=chol,color=disease),position = "jitter")

ggplot(data) +
  geom_density2d(aes(x=age,y=chol),color="lightblue3",size=1.2)

ggplot(data[data$disease != "no disease", ])+
  geom_histogram(aes(x=chol),fill="lightblue3",bins=15)

ggplot(data[data$disease != "disease", ])+
  geom_histogram(aes(x=chol),fill="lightblue3",bins=15)

table(data$sex_char,data$exang)

ggplot(data)+
  geom_bar(aes(x=sex_char,fill=as.character(exang)),position = 'dodge')

z<-ifelse(data$fbs == 0,'fbs<120 mg/dl', 'fbs>120 mg/dl')
data$fbs_char = z
pie_fbs = ggplot(data, aes(x="", y=fbs_char, fill= fbs_char)) + geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0)
pie_fbs

table(data$sex_char,data$fbs)

ggplot(data)+
  geom_bar(aes(x=sex_char,fill=as.character(fbs)),position = 'dodge')

data[c(0:-1)]
head(age, sex, cp, trestbps, chol, fbs, restecg, thalach, exang, oldpeak, slope, ca, thal)





