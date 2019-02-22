install.packages("ddply")
install.packages("randomForest")
install.packages("ranger")
install.packages("gbm")
install.packages("ggplot")
library(randomForest)
library(rnorm2)
library(RWeka)
library(stats)
library(pscl)
library(easynls)
library(data.table)
library(ranger)
library(hexbin)
library(RColorBrewer)
require(count)
require(MASS)
require(gbm)
#Loading Left Handed Batter and Right Handed Pitcher dataset
lrdf<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/lrdf.rds")

#Sample Data
lrdfsample <- lrdf[sample(1:nrow(lrdf), 40000,
                          replace=FALSE),]

scatter.smooth(y=lrdfsample$fantasypoints, x=lrdfsample$woba)

buylrd = c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
           "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026") 

myColRamp = colorRampPalette(c(buylrd))

smoothScatter(x=lrdf$rollingpa_10s,y=lrdf$fantasypoints,
              colramp=myColRamp,
              xlab="x1",
              ylab="x2",
              ylim=c(0,80))

lrdf$rollingpa_10s<-lrdf$rollingpa_10^2

cor(lrdf$rollingpa_10, lrdf$fantasypoints, use = "complete.obs")
cor(lrdf$rollingpa_10s, lrdf$fantasypoints, use = "complete.obs")



lrdf$TEMP_PARK_CTs<-lrdf$TEMP_PARK_CT^2

lrdf$temp<-ifelse(lrdf$TEMP_PARK_CT>=70,'hot','cold')
                  
lrdf$temp<-as.factor(lrdf$temp)

lrdf$BAT_LINEUP_ID<-as.factor(lrdf$BAT_LINEUP_ID)
lrdf$WIND_DIRECTION_PARK_CD<-as.factor(lrdf$WIND_DIRECTION_PARK_CD)
lrdf$BAT_HOME_ID<-as.factor(lrdf$BAT_HOME_ID)
lrdf$DAY<-substr(lrdf$GAME_ID, 4, 12)

lrdf$fpbin<-ifelse(lrdf$fantasypoints>0,1,0)
unique<-as.data.frame(unique(lrdftrain$fantasypoints))
uni_count<-as.data.frame(aggregate(data.frame(count = lrdftrain$fantasypoints), list(value = lrdftrain$fantasypoints), length))
lrdf <- lrdf[order( lrdf$bat_id, lrdf$Game),]

lrdf$slg<-(lrdf$rollingsingles+2*lrdf$rollingdoubles+3*lrdf$rollingtriples+4*lrdf$rollinghr)/lrdf$rollingab
#Doesn't take Sacrafice Flies into consideration
lrdf$obp<-(lrdf$rollinghits+lrdf$rollingubb+lrdf$rollingibb)/(lrdf$rollingab+lrdf$rollingubb+lrdf$rollingibb)
lrdf$ops<-lrdf$slg+lrdf$obp
lrdf$int<-lrdf$bat_id*lrdf$stadium

#setDT(lrdf)[,lagfantasypoints := shift(fantasypoints ,type = "lag"), by = list(bat_id)]

#lrdf<-na.omit(lrdf)
#indexes = sample(1:nrow(lrdf), size=0.2*nrow(lrdf))
#lrdftest<- lrdf[indexes,]
#lrdftrain<- lrdf[-indexes,]
lrdftest<-lrdf[which(lrdf$DAY==201510040),]
lrdftrain<-lrdf[which(lrdf$DAY!=201510040),]

which(lrdftrain$fpbin==0)

num<-subset(lrdf,select=-c(bat_id, GAME_ID, Game, BAT_HAND_CD, PIT_HAND_CD,YEAR_ID, stadium, BAT_LINEUP_ID))

correlations<-as.data.frame(cor(num, use = "complete.obs"))

#Negative Binomial
m1 <- glm.nb(fantasypoints ~ BAT_LINEUP_ID + stadium+rollingballs_10+
               rollingpa_10+rollinghits_10+eb10+woba10+iso10+temp+stadium*temp, data = lrdftrain)
summary(m1)
p<-as.data.frame(predict(m1, lrdftest, type = "response"))
acc <- data.frame(cbind(actuals=lrdftest$fantasypoints, predicteds=p))
cor(p, lrdftest$fantasypoints, use = "complete.obs")
hist(lrdftest$fantasypoints)
hist(acc$predict.m1..lrdftest..type....response..)
hist(lrdftest$rollinghits)


#Model Regression
m5 <- M5P(fantasypoints ~ BAT_LINEUP_ID + stadium+rollingballs_10+
            rollingpa_10+rollinghits_10+eb10+woba10+iso10+TEMP_PARK_CT, data = lrdftrain)
summary(m5)
p<-as.data.frame(predict(m5, lrdftest))
acc <- data.frame(cbind(actuals=lrdftest$fantasypoints, predicteds=p))
cor(p, lrdftest$fantasypoints, use = "complete.obs")
hist(acc$predict.m5..lrdftest.)

#GLM
glm <- glm(fantasypoints ~ BAT_LINEUP_ID + stadium+rollingballs_10+
          rollingpa_10+rollinghits_10+eb10+woba10+iso10,data=lrdftrain, family=negative.binomial(10))
p<-as.data.frame(predict(glm, lrdftest, type = "response"))
acc <- data.frame(cbind(actuals=lrdftest$fantasypoints, predicteds=p))
cor(p, lrdftest$fantasypoints, use = "complete.obs")
hist(p$`predict(glm, lrdftest, type = "response")`)



#LOESS Regression
lo<-loess(fantasypoints ~ rollingpa+rollinghits+woba+iso, lrdftrain)

#Logistic Regression
log <- glm(fpbin ~ BAT_LINEUP_ID*temp + stadium+rollingballs_10+
       rollingpa_10+rollinghits_10+eb10+woba10+iso10+temp+stadium*temp,
       family=binomial(link='logit'),data=lrdftrain)
summary(log)

p<-as.data.frame(predict(log, lrdftest, type = "response"))
p$predictbin<-ifelse(p$`predict(log, lrdftest, type = "response")`>0.50,1,0)
acc <- data.frame(cbind(actuals=lrdftest$fpbin, predicteds=p$predictbin))

acc2<- data.frame(cbind(actuals=lrdftest$fantasypoints, predicteds=p))
acc3<-acc2[which(acc2$actuals>0),]
somersd <-function(df,p)
{

  
}

somersd(lrdf,fantasypoints)
colnames(acc2)[2]<-"p"

concordant<-0
discordant<-0
for(i in 2:nrow(acc3))
{
if(acc3$p[i]>acc3$p[i-1] & acc3$actuals[i]>=acc3$actuals[i-1] |(acc3$p[i]<acc2$p[i-1] & acc3$actuals[i]<=acc3$actuals[i-1])){
concordant<-concordant+1
}
if(acc3$p[i]>acc3$p[i-1] & acc3$actuals[i]<acc3$actuals[i-1] |(acc3$p[i]<acc3$p[i-1] & acc3$actuals[i]>acc3$actuals[i-1])){
discordant<-discordant+1}
}

(concordant-discordant)/(concordant+discordant)

lrdf<-sort(lrdf[,lrdf$fantasypoints])
error <- lrdftest[which(acc$actuals !=acc$predicteds),]
correct<- lrdftest[which(acc$actuals ==acc$predicteds),]
lrdftestpointserror<-acc[which(acc$actuals==1 & acc$actuals !=acc$predicteds),]
lrdftestpointscorrect<-acc[which(acc$actuals==1 & acc$actuals==acc$predicteds),]

#Random Forest
rf <- ranger(fpbin ~ BAT_LINEUP_ID+BAT_HOME_ID+stadium+rollingballs_10+
      rollingpa_10+rollinghits_10+eb10+woba10+iso10+TEMP_PARK_CT+
      WIND_SPEED_PARK_CT, data =lrdftrain, num.trees = 200, write.forest = TRUE)

p1<-predict(rf, lrdftest, type = "response")
p1<-as.data.frame(p1$predictions)
p$predictbin<-ifelse(p$`p$predictions`>0.50,1,0)
acc <- data.frame(cbind(actuals=lrdftest$fpbin, predicteds=p$predictbin))
acc2 <- data.frame(cbind(actuals=lrdftest$fantasypoints, predicteds=p$`p$predictions`))

error <- lrdftest[which(acc$actuals !=acc$predicteds),]
correct<- lrdftest[which(acc$actuals ==acc$predicteds),]

lrdftestpointserror<-acc[which(acc$actuals==1 & acc$actuals !=acc$predicteds),]
lrdftestpointscorrect<-acc[which(acc$actuals==1 & acc$actuals==acc$predicteds),]

comb<-data.frame(cbind(logistic=p$`predict(log, lrdftest, type = "response")`, rf=p1$`p1$predictions`))

cor(comb$logistic, comb$rf)

#Gradient Boosting
gbm<-gbm(fpbin ~ BAT_LINEUP_ID*temp + stadium+rollingballs_10+
        rollingpa_10+rollinghits_10+eb10+woba10+iso10+temp+stadium*temp, 
        distribution = "bernoulli", data = lrdftrain, n.trees = 10000, 
        interaction.depth = 2, shrinkage = 0.01)

p2<-as.data.frame(predict(gbm, lrdftest, 100, type = "response"))
p2$predictbin<-ifelse(p2$`predict(gbm, lrdftest, 10, type = "response")`>0.50,1,0)
acc <- data.frame(cbind(actuals=lrdftest$fpbin, predicteds=p2$predictbin))
error <- lrdftest[which(acc$actuals !=acc$predicteds),]
correct<- lrdftest[which(acc$actuals ==acc$predicteds),]