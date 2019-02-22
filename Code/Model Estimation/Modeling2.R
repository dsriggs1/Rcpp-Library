library(randomForest)
library(rnorm2)
library(RWeka)
library(stats)
library(pscl)
library(easynls)
library(data.table)
library(ranger)
library(h2o)
require(count)
require(MASS)
require(gbm)
h2o.shutdown()
h2o.init()
#Loading Left Handed Batter and Right Handed Pitcher dataset
lrdf<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/lrdf.rds")
lrdf$fpbin<-ifelse(lrdf$fantasypoints>0,1,0)
lrdf$DAY<-substr(lrdf$GAME_ID, 4, 12)
lrdf$temp<-ifelse(lrdf$TEMP_PARK_CT>=70,'hot','cold')

lrdf$temp<-as.factor(lrdf$temp)
lrdf$BAT_LINEUP_ID<-as.factor(lrdf$BAT_LINEUP_ID)

ldrdf<-subset(lrdf, select=c(fpbin, BAT_LINEUP_ID, BAT_HOME_ID, stadium, rollingballs_10,
                                rollingpa_10, rollinghits_10, eb10, woba10, iso10, TEMP_PARK_CT,
                                WIND_SPEED_PARK_CT, DAY))
lrdf<-na.omit(lrdf)                     
lrdftest<-as.h2o(lrdf[which(lrdf$DAY==201510040),])
lrdftrain<-as.h2o(lrdf[which(lrdf$DAY!=201510040),])   
lrdftest<-lrdf[which(lrdf$DAY==201510040),]

nfolds <- 5

rf <- h2o.randomForest(x=c("BAT_LINEUP_ID", "stadium","rollingballs_10",
                           "rollingpa_10","rollinghits_10","eb10","woba10","iso10","temp"),
                       y="fpbin",
                          training_frame = lrdftrain,
                          ntrees = 200,
                          nfolds = nfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)
gbm <- h2o.gbm(x=c("BAT_LINEUP_ID", "stadium","rollingballs_10",
                   "rollingpa_10","rollinghits_10","eb10","woba10","iso10","temp"),
               y="fpbin",
                  training_frame = lrdftrain,
                  ntrees = 200,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  max_depth = 3,
                  min_rows = 2,
                  learn_rate = 0.2)
                            
#Correct way to specify variables
log <- h2o.glm(x=c("BAT_LINEUP_ID", "stadium","rollingballs_10",
                 "rollingpa_10","rollinghits_10","eb10","woba10","iso10","temp"),
               y="fpbin",
              nfolds = nfolds,
              fold_assignment = "Modulo",
              keep_cross_validation_predictions = TRUE,
              training_frame=lrdftrain)

nn<-h2o.deeplearning(x=c("BAT_LINEUP_ID", "stadium","rollingballs_10",
                         "rollingpa_10","rollinghits_10","eb10","woba10","iso10","temp"), y="fpbin",nfolds = nfolds,
                     fold_assignment = "Modulo", 
                     keep_cross_validation_predictions = TRUE,
                     lrdftrain)

ensemble <- h2o.stackedEnsemble(x=c("BAT_LINEUP_ID", "stadium","rollingballs_10",
                                    "rollingpa_10","rollinghits_10","eb10","woba10","iso10","temp"),
                                y="fpbin",
                                training_frame = lrdftrain,
                                model_id = "Ensemble",
                                base_models = list(rf, gbm, log, nn))

p <- as.data.frame(h2o.predict(rf, newdata = lrdftest))
p2 <- as.data.frame(h2o.predict(gbm, newdata = lrdftest))
p3 <- as.data.frame(h2o.predict(log, newdata = lrdftest))
p4 <- as.data.frame(h2o.predict(nn, newdata = lrdftest))
p5 <- as.data.frame(h2o.predict(ensemble, newdata = lrdftest))

acc <- data.frame(cbind(actuals=lrdftest$fantasypoints, predicteds=p5$predict))
pred<-cbind(p,p2,p3, p4)
cor(pred)

concordant<-0
discordant<-0
for(i in 2:nrow(acc))
{
  if(acc$p[i]>acc$p[i-1] & acc$actuals[i]>=acc$actuals[i-1] |(acc$p[i]<acc$p[i-1] & acc$actuals[i]<=acc$actuals[i-1])){
    concordant<-concordant+1
  }
  if(acc$p[i]>acc$p[i-1] & acc$actuals[i]<acc$actuals[i-1] |(acc$p[i]<acc$p[i-1] & acc$actuals[i]>acc$actuals[i-1])){
    discordant<-discordant+1}
}

(concordant-discordant)/(concordant+discordant)