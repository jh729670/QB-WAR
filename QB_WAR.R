library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)
library(caret)
#Reads the data
QB_data<-read.csv('QB_data_wins.csv', header=TRUE)
#Checks names of columns
names(QB_data)
#Filters out QB's who played in less than six games
QB_df<-QB_data%>%
  filter(GP>=6)
#Groups data by Avg_wins 
avg_Wins<-group_by(QB_df,Year)
#Creates tibble of Avg Wins per Year
Wins_Year<-summarize(avg_Wins, mean_Wins=mean(Wins,na.rm=TRUE))
#Gets the avg Points per Year
avg_Points<-group_by(QB_df,Year)
Points_Year<-summarize(avg_Points,mean_Points=mean(Points,na.rm=TRUE))
#Columns I need to perform calculations
col_names<-QB_df[,c(2,4,6,9,10,12,19,24,34,35,36)]
#group_by Year
group<-group_by(col_names,Year)%>%
  filter(GP>=6)
#Get Mean for each category by year
Comp_year<-summarize(group,mean_Comp=mean(Pass_Comp, na.rm=TRUE))
Percent_year<-summarize(group,mean_Per=mean(Comp_.))
Yards_year<-summarize(group,mean_Yards=mean(Pass.Yards))
TD_year<-summarize(group,mean_TD=mean(Total_TD))
TO_year<-summarize(group,mean_TO=mean(Total_TOs))
Perform_year<-summarize(group,mean_Perform=mean(Perfromance_Metric))
Rush_year<-summarize(group,mean_Rush=mean(Rush_Yards))

#Combine into a dataframe
Avg_df<-as.data.frame(cbind(Perform_year,TO_year[,2],TD_year[,2],Yards_year[,2],
      Percent_year[,2],Comp_year[,2],Points_Year[,2],Wins_Year[,2],Rush_year[,2]))

QB_df<-mutate(QB_df,WAR=ifelse(Year==2019,((Pass_Comp-Avg_df[6,7])+((Comp_.-Avg_df[6,6])*100)+
                                 ((Pass.Yards-Avg_df[6,5])/100)+(Total_TD-Avg_df[6,4])+
                                 (Perfromance_Metric-Avg_df[6,2])+(Personal.Wins-Avg_df[6,9])-
                                 (Total_TOs-Avg_df[6,3])+(Points-Avg_df[6,8])+
                                 ((Rush_Yards-Avg_df[6,10])/100))/Avg_df[6,11],
                                  ifelse(Year==2018,((Pass_Comp-Avg_df[5,7])+((Comp_.-Avg_df[5,6])*100)+
                                  ((Pass.Yards-Avg_df[5,5])/100)+(Total_TD-Avg_df[5,4])+
                                  (Perfromance_Metric-Avg_df[5,2])+(Personal.Wins-Avg_df[5,9])-
                                  (Total_TOs-Avg_df[5,3])+(Points-Avg_df[5,8])+
                                  ((Rush_Yards-Avg_df[5,10])/100))/Avg_df[5,11],
                                  ifelse(Year==2017,((Pass_Comp-Avg_df[4,7])+((Comp_.-Avg_df[4,6])*100)+
                                  ((Pass.Yards-Avg_df[4,5])/100)+(Total_TD-Avg_df[4,4])+
                                  (Perfromance_Metric-Avg_df[4,2])+(Personal.Wins-Avg_df[4,9])-
                                  (Total_TOs-Avg_df[4,3])+(Points-Avg_df[4,8])+
                                  ((Rush_Yards-Avg_df[4,10])/100))/Avg_df[4,11],
                                  ifelse(Year==2016,((Pass_Comp-Avg_df[3,7])+((Comp_.-Avg_df[3,6])*100)+
                                  ((Pass.Yards-Avg_df[3,5])/100)+(Total_TD-Avg_df[3,4])+
                                  (Perfromance_Metric-Avg_df[3,2])+(Personal.Wins-Avg_df[3,9])-
                                  (Total_TOs-Avg_df[3,3])+(Points-Avg_df[3,8])+
                                  ((Rush_Yards-Avg_df[3,10])/100))/Avg_df[3,11],
                                  ifelse(Year==2015,((Pass_Comp-Avg_df[2,7])+((Comp_.-Avg_df[2,6])*100)+
                                  ((Pass.Yards-Avg_df[2,5])/100)+(Total_TD-Avg_df[2,4])+
                                  (Perfromance_Metric-Avg_df[2,2])+(Personal.Wins-Avg_df[2,9])-
                                  (Total_TOs-Avg_df[2,3])+(Points-Avg_df[2,8])+
                                  ((Rush_Yards-Avg_df[2,10])/100))/Avg_df[2,11],
                                  ((Pass_Comp-Avg_df[1,7])+((Comp_.-Avg_df[1,6])*100)+
                                  ((Pass.Yards-Avg_df[1,5])/100)+(Total_TD-Avg_df[1,4])+
                                  (Perfromance_Metric-Avg_df[1,2])+(Personal.Wins-Avg_df[1,9])-
                                  (Total_TOs-Avg_df[1,3])+(Points-Avg_df[1,8])+
                                  ((Rush_Yards-Avg_df[1,10])/100))/Avg_df[1,11]))))))


Avg_df<-mutate(Avg_df,avg_game=(mean_Points/16))
class(QB_df)

ggplot(QB_df,aes(x=WAR,y=Personal.Wins))+geom_point()+stat_smooth(method='lm', se=FALSE)
#Plots WAR Against Fantasy Points where WAR >0
ggplot(data=QB_df[QB_df$WAR>=0,], aes(WAR, Fantasy_Points))+geom_point(col='red', size=2)+
  stat_smooth(method='lm',se=FALSE)

#Plots density, to view for any outliers. Looks pretty normal
ggplot(QB_df[QB_df$GP>=14,], aes(WAR))+geom_density() 

#Look for R value-Adj R^2=.8062. Not bad
lm_model=lm(WAR~Fantasy_Points,QB_df)
summary(lm_model)

#Measure Error on Train and Test
sample_df<-sample(1:nrow(QB_df),.8*nrow(QB_df))
train_df<-QB_df[sample_df,]
test_df<-QB_df[-sample_df,]
lm_model_train<-lm(WAR~Fantasy_Points,train_df)
prediction<-predict(lm_model_train,test_df)
#RMSE value of 2.88
RMSE(prediction,test_df$WAR)
#R2 value of .845
R2(prediction,test_df$WAR)


