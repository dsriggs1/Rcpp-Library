
#Loading datasets
lldf<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/lrdf.rds")
lrdf<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/lrdf.rds")
rldf<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/lrdf.rds")
rrdf<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/lrdf.rds")

variables <- as.list(lrdf[,28:30])
names<-colnames(lrdf[,28:30])

scatter.smooth(y=lrdf$fantasypoints, x=lrdf$rollinghits_1)

for(i in 1:length(variables))
{
  name<-names[[i]]
  png(file ='C:/Users/Sean/Documents/R_Baseball_Project/Charts/name.png')
  scatter.smooth(y=lrdf$fantasypoints, x=variables[[i]], xlab=names[[i]], main=names[[i]])
  #print (paste0(lrdf$woba,i))
  #print (lrdf$woba[i])
  
}

scatter.smooth(y=lrdf$fantasypoints, x=lrdf$rollingab_1)


lrdf$temp<-ifelse(lrdf$TEMP_PARK_CT>=20 & lrdf$TEMP_PARK_CT<=29,20,
                  ifelse(lrdf$TEMP_PARK_CT>=30 & lrdf$TEMP_PARK_CT<=39,30,
                  ifelse(lrdf$TEMP_PARK_CT>=40 & lrdf$TEMP_PARK_CT<=49,40,
                  ifelse(lrdf$TEMP_PARK_CT>=50 & lrdf$TEMP_PARK_CT<=59,50,
                  ifelse(lrdf$TEMP_PARK_CT>=60 & lrdf$TEMP_PARK_CT<=69,60,
                  ifelse(lrdf$TEMP_PARK_CT>=70 & lrdf$TEMP_PARK_CT<=79,70,
                  ifelse(lrdf$TEMP_PARK_CT>=80 & lrdf$TEMP_PARK_CT<=89,80,
                  90)))))))
lrdf$temp<-as.factor(lrdf$temp)

stadiummean<-as.data.frame(aggregate(lrdf$fantasypoints, list(lrdf$stadium), mean))
tempmean<-as.data.frame(aggregate(lrdf$fantasypoints, list(lrdf$temp), mean))
tempstadmean<-as.data.frame(aggregate(lrdf$fantasypoints, list(lrdf$temp, lrdf$stadium), mean))
winddirstadmean<-as.data.frame(aggregate(lrdf$fantasypoints, list(lrdf$WIND_DIRECTION_PARK_CD, lrdf$stadium, lrdf$temp), mean))

winddirstadlength<-as.data.frame(aggregate(lrdf$fantasypoints, list(lrdf$WIND_DIRECTION_PARK_CD, lrdf$stadium, lrdf$temp), length))

windirstad<-merge(winddirstadmean, winddirstadlength, by=c("Group.1" , "Group.2", "Group.3"))