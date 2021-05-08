company=read.csv(file.choose())
View(company)
company$Sales=cut(company$Sales,c(0,5,10,15),labels = c('low','avg','high'))
View(company)
summary(company)
sum(is.na(company))
boxplot(company)
plot(company)
### visuvalization with density plot
library(ggplot2)
ggplot(data=company,aes(x=company$Sales,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for sales varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
###
ggplot(data=company,aes(x=company$CompPrice,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for comprice varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Income,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for income varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Advertising,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for Advertising varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Population,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for population varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Price,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for price varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$ShelveLoc,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for Shelveloc varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Age,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for age varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Education,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for Education varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$Urban,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for urban varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=company,aes(x=company$US,fill=company$Sales))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title = 'company data for US varable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
######
library(caret)
library(C50)
##data partition for model building and testing
inTraininglocal=createDataPartition(company$Sales,p=.75,list = F)
training=company[inTraininglocal,]
testing=company[-inTraininglocal,]
#model building
model=C5.0(training$Sales~.,data=training)
#genrrateting the model summary
summary(model)
pred=predict.C5.0(model,testing[,-1])##split the diagolas of data in testing
pred
a=table(testing$Sales,pred)
a
sum(diag(a)/sum(a))
plot(model)
