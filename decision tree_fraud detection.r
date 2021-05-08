fraud=read.csv(file.choose())
View(fraud)
fraud$Taxable.Income<- ifelse(fraud$Taxable.Income<=30000,"risky","good")
View(fraud)
summary(fraud)
sum(is.na(fraud))
str(fraud)
###visualization on density plot
library(ggplot2)
ggplot(data=fraud,aes(x=fraud$Undergrad,fill=fraud$Taxable.Income))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'fraud data for undergrad varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=fraud,aes(x=fraud$Marital.Status,fill=fraud$Taxable.Income))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'fraud data for Matrital.status varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=fraud,aes(x=fraud$Taxable.Income,fill=fraud$Taxable.Income))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'fraud data for Taxebel.income varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=fraud,aes(x=fraud$City.Population,fill=fraud$Taxable.Income))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'fraud data for city.population varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=fraud,aes(x=fraud$Work.Experience,fill=fraud$Taxable.Income))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'fraud data for work.Experience varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=fraud,aes(x=fraud$Urban,fill=fraud$Taxable.Income))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'fraud data for urben varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))

library(caret)
library(C50)
##data partion for model building and testing
inTraininglocal=createDataPartition(fraud$Taxable.Income,p=.75,list = F)
training=fraud[inTraininglocal,]
testing=fraud[-inTraininglocal,]
#model building
str(training$Taxable.Income)
training$Taxable.Income <- as.factor(training$Taxable.Income)
model=C5.0(training$Taxable.Income~.,data = training)
#generate a model of summary
summary(model)
pred=predict.C5.0(model,testing[-3])
pred
a=table(testing$Taxable.Income,pred)
a
sum(diag(a)/sum(a))
plot(model)
