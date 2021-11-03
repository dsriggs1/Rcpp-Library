install.packages("zoo")
install.packages("pscl")
install.packages("HEAT")
install.packages("speedglm")
install.packages("caret")
install.packages("tree")
install.packages("rpart.plot")
install.packages("RWeka")
install.packages("partykit")
install.packages("rJava", type = 'source')
install.packages("dplyr")
install.packages("data.table")
library(partykit)
library(RWeka)
library(rpart.plot)
library(rpart)
library(tree)
library(caret)
library(speedglm)
library(XML)
library(xlsx)
library(plyr)
library(rvest)
library(RODBC)
library(dplyr)
library("biglm")
library("h2o")
library("neuralnet")
library("zoo")
library("HEAT")
library(parallel)
library(data.table)
library(dplyr)
require(parallel)
require(MASS)
require(zoo)
require(pscl)
require(caret)
ptm <- proc.time()
# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

h2o.init(nthreads = -1)
db <- odbcConnect("retrosheet", uid="root", pwd="abcdefgh1!")
channel <- odbcConnect("retrosheet", uid="root")

events<-sqlQuery(channel, paste("select seq_events, YEAR_ID, GAME_ID, bat_id, PA_BALL_CT, PA_SWINGMISS_STRIKE_CT, BAT_HOME_ID, BAT_FATE_ID, EVENT_CD, RBI_CT, event_tx, BAT_LINEUP_ID, 
                            BASE1_RUN_ID, BASE2_RUN_ID, BASE3_RUN_ID,RUN1_SB_FL,RUN2_SB_FL, RUN3_SB_FL, ab_fl, 
                            PR_Run1_fl, PR_Run2_fl, PR_Run3_fl, INN_CT, 
                            AWAY_SCORE_CT, HOME_SCORE_CT, HOME_TEAM_ID, BAT_HAND_CD, PIT_HAND_CD, PIT_ID 
                            from events 
                            where year_id>='2016'"))

weather<-sqlQuery(channel, paste("select year_id, GAME_ID, TEMP_PARK_CT, WIND_DIRECTION_PARK_CD, WIND_SPEED_PARK_CT, 
                             PRECIP_PARK_CD, SKY_PARK_CD,WIN_PIT_ID
                            from games 
                            where year_id>='2016'"))

df<-merge(events,weather)

df<-distinct(df)

#Adding variables for the impact of individual events on fantasy points
#Batter Points
single=3
double=5
triple=8
hr=10
rbi=2
run=2
walk=2
hbp=2
sb=5

#Pitcher Points
ip=2.25 #innings pitched
so=2 #strikeout
win=4
era=-2 #earned run allowed
ha=-0.6 #hit against (allowed a hit)
walk=-0.6
hb=-0.6 #hit batter
cg=2.5 #complete game
cgs=2.5 #complete game shutout
nh=5 #no hitter

#Creating Flag for Hits
df$hit_fl<-df$EVENT_CD == "20"|df$EVENT_CD == "21"|df$EVENT_CD == "22"|df$EVENT_CD=="23"

#Computing Pitching Points (Draft Kings)
df$pitchingpoints<- ifelse(df$EVENT_CD==3,2,ifelse(df$hit_fl==TRUE,-0.6,
                    ifelse(df$EVENT_CD==14,-0.6,
                    ifelse(df$EVENT_CD==16,-0.6,0))))

#Computing Batting Points
df$fantasypoints <- ifelse(df$EVENT_CD==23, 12,ifelse(df$EVENT_CD==20,3+df$RBI_CT*(3),
                    ifelse(df$EVENT_CD==21,6+df$RBI_CT*(3),
                    ifelse(df$EVENT_CD==14,3+df$RBI_CT*(3),
                    ifelse(df$EVENT_CD==16,3+df$RBI_CT*(3),
                    ifelse(df$EVENT_CD==22,9+df$RBI_CT*(3),0))))))

df$stadium <- ifelse(df$HOME_TEAM_ID=="ANA","Angel Stadium of Anaheim",
              ifelse(df$HOME_TEAM_ID=="ARI","Chase Field",
              ifelse(df$HOME_TEAM_ID=="ATL","Turner Field",     
              ifelse(df$HOME_TEAM_ID=="BAL","Oriole Park at Camden Yards",
              ifelse(df$HOME_TEAM_ID=="BOS","Fenway Park",
              ifelse(df$HOME_TEAM_ID=="CHA","Guaranteed Rate Field",
              ifelse(df$HOME_TEAM_ID=="CHN","Wrigley Field",
              ifelse(df$HOME_TEAM_ID=="CIN","Great American Ball Park",      
              ifelse(df$HOME_TEAM_ID=="CLE","Progressive Field",               
              ifelse(df$HOME_TEAM_ID=="COL","Coors Field",       
              ifelse(df$HOME_TEAM_ID=="DET","Comerica Park",
              ifelse(df$HOME_TEAM_ID=="HOU","Minute Maid Park",
              ifelse(df$HOME_TEAM_ID=="KCA","Kauffman Stadium",
              ifelse(df$HOME_TEAM_ID=="LAN","Dodger Stadium",
              ifelse(df$HOME_TEAM_ID=="MIA","Marlins Park",
              ifelse(df$HOME_TEAM_ID=="MIL","Miller Park",
              ifelse(df$HOME_TEAM_ID=="MIN","Target Field",
              ifelse(df$HOME_TEAM_ID=="NYA","Yankee Stadium",
              ifelse(df$HOME_TEAM_ID=="NYN","Citi Field",
              ifelse(df$HOME_TEAM_ID=="OAK","Oakland Coliseum",
              ifelse(df$HOME_TEAM_ID=="PHI","Citizens Bank Park",
              ifelse(df$HOME_TEAM_ID=="PIT","PNC Park",
              ifelse(df$HOME_TEAM_ID=="SDN","Petco Park",
              ifelse(df$HOME_TEAM_ID=="SEA","Safeco Field",
              ifelse(df$HOME_TEAM_ID=="SFN","AT&T Park",
              ifelse(df$HOME_TEAM_ID=="SLN","Busch Stadium",
              ifelse(df$HOME_TEAM_ID=="TBA","Tropicana Field",
              ifelse(df$HOME_TEAM_ID=="TEX","Globe Life Park in Arlington",
              ifelse(df$HOME_TEAM_ID=="TOR","Rogers Centre",
              ifelse(df$HOME_TEAM_ID=="WAS","Nationals Park",0))))))))))))))))))))))))))))))

df$stadium <- as.factor(df$stadium)
  
#Computing Run Scored Points
df$fantasypoints <- ifelse(df$BAT_FATE_ID==4,df$fantasypoints+3+df$RBI_CT*(3),df$fantasypoints)

#Computing Stolen Base Points
sb<-as.data.frame(df[which(df$EVENT_CD==4),])

sb$BASE_STL_ID<-ifelse(sb$RUN1_SB_FL==TRUE & sb$RUN2_SB_FL==TRUE, paste(sb$BASE1_RUN_ID, sb$BASE2_RUN_ID),
                ifelse(sb$RUN1_SB_FL==TRUE & sb$RUN3_SB_FL==TRUE, paste(sb$BASE1_RUN_ID, sb$BASE3_RUN_ID),
                ifelse(sb$RUN2_SB_FL==TRUE & sb$RUN3_SB_FL==TRUE, paste(sb$BASE2_RUN_ID, sb$BASE3_RUN_ID),
                ifelse(sb$RUN1_SB_FL==TRUE, as.character(sb$BASE1_RUN_ID), 
                ifelse(sb$RUN2_SB_FL==TRUE, as.character(sb$BASE2_RUN_ID),
                as.character(sb$BASE3_RUN_ID))))))

sb<-separate_rows(sb,BASE_STL_ID,sep=" ")

sb$STOLEN_BASE_POINTS<-3

sb<-setNames(aggregate(sb$STOLEN_BASE_POINTS, by=list(sb$GAME_ID, sb$BASE_STL_ID), FUN=sum)
             , c("GAME_ID", "BASE_STL_ID", "STOLEN_BASE_POINTS"))

proc.time() - ptm


df$Game <- substr(df$GAME_ID,4,12)
df <- df[order(df$Game, df$bat_id),]

#Calculating Batting Average
df$hits <- ave(df$hit_fl,df$bat_id, df$YEAR_ID, FUN=cumsum)
df$at_bats <-ave(df$ab_fl=="TRUE", df$bat_id, df$YEAR_ID, FUN=cumsum)
df$batting_average=df$hits/df$at_bats
df$batting_average <- format(round(df$batting_average, 3), nsmall = 2)
df$batting_average <- as.numeric(df$batting_average)

#Calculating 1st Gen. Batting Stats
df$gamehits <- ave(df$hit_fl=="TRUE", df$bat_id, df$GAME_ID, df$PIT_HAND_CD, FUN=cumsum)
df$gameab <- ave(df$ab_fl=="TRUE", df$bat_id, df$GAME_ID, df$PIT_HAND_CD, FUN=cumsum)
df$gameba <- df$gamehits/df$gameab
df$gamesingles<-ave(df$EVENT_CD == "20", df$bat_id, df$GAME_ID, df$PIT_HAND_CD, FUN=cumsum)
df$gamedoubles<-ave(df$EVENT_CD == "21", df$bat_id, df$GAME_ID, df$PIT_HAND_CD, FUN=cumsum)
df$gametriples<-ave(df$EVENT_CD == "22", df$bat_id, df$GAME_ID, df$PIT_HAND_CD, FUN=cumsum)
df$gamehr<-ave(df$EVENT_CD == "23", df$bat_id, df$GAME_ID, df$PIT_HAND_CD, FUN=cumsum)
df$gamepa<-ave(df$ab_fl==TRUE|df$EVENT_CD==14|df$EVENT_CD==15|df$EVENT_CD==16, df$bat_id, df$GAME_ID, df$PIT_HAND_CD, FUN=cumsum)
df$gameibb<-ave(df$EVENT_CD==15, df$bat_id, df$GAME_ID, df$PIT_HAND_CD, FUN=cumsum)
df$gameubb<-ave(df$EVENT_CD==14, df$bat_id, df$GAME_ID, df$PIT_HAND_CD, FUN=cumsum)
df$gameballs<-ave(df$PA_BALL_CT, df$bat_id, df$GAME_ID, df$PIT_HAND_CD, FUN=cumsum)
df$gameswingstrikes<-ave(df$PA_SWINGMISS_STRIKE_CT, df$bat_id, df$GAME_ID, df$PIT_HAND_CD, FUN=cumsum)

#Aggregating from AtBat to Game level
dfg <-aggregate(list(fantasypoints=df$fantasypoints), by=list(GAME_ID=df$GAME_ID, Game=df$Game, bat_id=df$bat_id,
                                                              BAT_HAND_CD=df$BAT_HAND_CD, PIT_HAND_CD=
                                                              df$PIT_HAND_CD, YEAR_ID=df$YEAR_ID), FUN=sum)
#Aggregating from AtBat to Game level Pitcher Points
dfgp <-aggregate(list(pitchingpoints=df$pitchingpoints), by=list(GAME_ID=df$GAME_ID, Game=df$Game, PIT_ID=df$PIT_ID,
                                                              BAT_HAND_CD=df$BAT_HAND_CD, PIT_HAND_CD=
                                                                df$PIT_HAND_CD, YEAR_ID=df$YEAR_ID), FUN=sum)
#Aggregating Game Level Ending Batting Statistics
dfg2<-aggregate(list(gameab=df$gameab, gamehits=df$gamehits,gamesingles=df$gamesingles,gamedoubles=df$gamedoubles,
                     gametriples=df$gametriples, gamehr=df$gamehr, gamepa=df$gamepa, gameibb=df$gameibb, 
                     gameubb=df$gameubb, PA_BALL_CT=df$PA_BALL_CT, gameballs=df$gameballs, gameswingstrikes=df$gameswingstrikes), 
                     by=list(GAME_ID=df$GAME_ID, Game=df$Game, bat_id=df$bat_id, PIT_ID=df$PIT_ID,
                     BAT_HAND_CD=df$BAT_HAND_CD, PIT_HAND_CD=df$PIT_HAND_CD, YEAR_ID=df$YEAR_ID), FUN=max)

#Merging Stolen Base points here
dfg<-merge(x=dfg, y=sb, by.x=c("GAME_ID", "bat_id"), by.y=c("GAME_ID","BASE_STL_ID"), all.x=TRUE)

#Getting Stadiums
dfgstadium<-aggregate(list(stadium=df$stadium,BAT_LINEUP_ID=df$BAT_LINEUP_ID,BAT_HOME_ID=df$BAT_HOME_ID, 
                           TEMP_PARK_CT=df$TEMP_PARK_CT, WIND_DIRECTION_PARK_CD=df$WIND_DIRECTION_PARK_CD, 
                           WIND_SPEED_PARK_CT=df$WIND_SPEED_PARK_CT, PRECIP_PARK_CD=df$PRECIP_PARK_CD, 
                           SKY_PARK_CD=df$SKY_PARK_CD),by=list(GAME_ID=df$GAME_ID, Game=df$Game, bat_id=df$bat_id,
                      BAT_HAND_CD=df$BAT_HAND_CD, PIT_HAND_CD=df$PIT_HAND_CD, YEAR_ID=df$YEAR_ID, 
                      WIN_PIT_ID=df$WIN_PIT_ID), FUN=last)

#Keeping Only Merge Criteria and New Batting Statistics
dfg2<-subset(dfg2,select= c(bat_id, GAME_ID, gamehits,gameab,gamesingles,gamedoubles,gametriples, 
                            gamehr, gamepa,gameibb, gameubb, BAT_HAND_CD,PIT_HAND_CD, gameballs, gameswingstrikes))

dfgstadium<-subset(dfgstadium, select=c(bat_id, GAME_ID, stadium, BAT_LINEUP_ID, BAT_HAND_CD,PIT_HAND_CD, BAT_HOME_ID,
                                        TEMP_PARK_CT, WIND_DIRECTION_PARK_CD, WIND_SPEED_PARK_CT, PRECIP_PARK_CD, 
                                        SKY_PARK_CD, WIN_PIT_ID))
dfg<-merge(dfg,dfg2, by=c("bat_id","GAME_ID","BAT_HAND_CD","PIT_HAND_CD"))
dfg<-merge(dfg,dfgstadium, by=c("bat_id","GAME_ID","BAT_HAND_CD","PIT_HAND_CD"))

dfg <- dfg[order(dfg$Game, dfg$bat_id),]

for(i in 1:10)
{
#Rolling Sums Going Into Game
setDT(dfg)[,(paste0("rollinghits_",i)) := shift(rollapply(gamehits,width=i,sum, fill=NA, align="right", partial=TRUE), type = "lag"), by = list(bat_id, PIT_HAND_CD)]
setDT(dfg)[,(paste0("rollingab_",i)) := shift(rollapply(gameab,width=i,sum, fill=NA, align="right", partial=TRUE), type = "lag"), by = list(bat_id, PIT_HAND_CD)]
setDT(dfg)[,(paste0("rollingpa_",i)) := shift(rollapply(gamepa,width=i,sum, fill=NA, align="right", partial=TRUE), type = "lag"), by = list(bat_id, PIT_HAND_CD)]
setDT(dfg)[,(paste0("rollingsingles_",i)) := shift(rollapply(gamesingles,width=i,sum, fill=NA, align="right", partial=TRUE), type = "lag"), by = list(bat_id, PIT_HAND_CD)]
setDT(dfg)[,(paste0("rollingdoubles_",i)) := shift(rollapply(gamedoubles,width=i,sum, fill=NA, align="right", partial=TRUE), type = "lag"), by = list(bat_id, PIT_HAND_CD)]
setDT(dfg)[,(paste0("rollingtriples_",i)) := shift(rollapply(gametriples,width=i,sum, fill=NA, align="right", partial=TRUE), type = "lag"), by = list(bat_id, PIT_HAND_CD)]
setDT(dfg)[,(paste0("rollinghr_",i)) := shift(rollapply(gamehr,width=i,sum, fill=NA, align="right", partial=TRUE), type = "lag"), by = list(bat_id, PIT_HAND_CD)]
setDT(dfg)[,(paste0("rollingubb_",i)) := shift(rollapply(gameubb,width=i,sum, fill=NA, align="right", partial=TRUE), type = "lag"), by = list(bat_id, PIT_HAND_CD)]
setDT(dfg)[,(paste0("rollingibb_",i)) := shift(rollapply(gameibb,width=i,sum, fill=NA, align="right", partial=TRUE), type = "lag"), by = list(bat_id, PIT_HAND_CD)]
setDT(dfg)[,(paste0("rollingballs_",i)) := shift(rollapply(gameballs,width=i,sum, fill=NA, align="right", partial=TRUE), type = "lag"), by = list(bat_id, PIT_HAND_CD)]
setDT(dfg)[,(paste0("rollingswingstrikes_", i)):= shift(rollapply( gameswingstrikes,width=i,sum, fill=NA, align="right", partial=TRUE), type = "lag"), by = list(bat_id, PIT_HAND_CD)]

#Lists
rollinghits<-list(dfg$rollinghits_1,dfg$rollinghits_2, dfg$rollinghits_3, dfg$rollinghits_4, dfg$rollinghits_5, dfg$rollinghits_6, dfg$rollinghits_7, dfg$rollinghits_8, dfg$rollinghits_9, dfg$rollinghits_10)
rollingab<-list(dfg$rollingab_1,dfg$rollingab_2, dfg$rollingab_3, dfg$rollingab_4, dfg$rollingab_5, dfg$rollingab_6, dfg$rollingab_7, dfg$rollingab_8, dfg$rollingab_9, dfg$rollingab_10)
rollingpa<-list(dfg$rollingpa_1,dfg$rollingpa_2, dfg$rollingpa_3, dfg$rollingpa_4, dfg$rollingpa_5, dfg$rollingpa_6, dfg$rollingpa_7, dfg$rollingpa_8, dfg$rollingpa_9, dfg$rollingpa_10)
rollingsingles<-list(dfg$rollingsingles_1,dfg$rollingsingles_2, dfg$rollingsingles_3, dfg$rollingsingles_4, dfg$rollingsingles_5, dfg$rollingsingles_6, dfg$rollingsingles_7, dfg$rollingsingles_8, dfg$rollingsingles_9, dfg$rollingsingles_10)
rollingdoubles<-list(dfg$rollingdoubles_1,dfg$rollingdoubles_2, dfg$rollingdoubles_3, dfg$rollingdoubles_4, dfg$rollingdoubles_5, dfg$rollingdoubles_6, dfg$rollingdoubles_7, dfg$rollingdoubles_8, dfg$rollingdoubles_9, dfg$rollingdoubles_10)
rollingtriples<-list(dfg$rollingtriples_1,dfg$rollingtriples_2, dfg$rollingtriples_3, dfg$rollingtriples_4, dfg$rollingtriples_5, dfg$rollingtriples_6, dfg$rollingtriples_7, dfg$rollingtriples_8, dfg$rollingtriples_9, dfg$rollingtriples_10)
rollinghr<-list(dfg$rollinghr_1,dfg$rollinghr_2, dfg$rollinghr_3, dfg$rollinghr_4, dfg$rollinghr_5, dfg$rollinghr_6, dfg$rollinghr_7, dfg$rollinghr_8, dfg$rollinghr_9, dfg$rollinghr_10)
rollingubb<-list(dfg$rollingubb_1,dfg$rollingubb_2, dfg$rollingubb_3, dfg$rollingubb_4, dfg$rollingubb_5, dfg$rollingubb_6, dfg$rollingubb_7, dfg$rollingubb_8, dfg$rollingubb_9, dfg$rollingubb_10)
rollingibb<-list(dfg$rollingibb_1,dfg$rollingibb_2, dfg$rollingibb_3, dfg$rollingibb_4, dfg$rollingibb_5, dfg$rollingibb_6, dfg$rollingibb_7, dfg$rollingibb_8, dfg$rollingibb_9, dfg$rollingibb_10)
rollingballs<-list(dfg$rollingballs_1,dfg$rollingballs_2, dfg$rollingballs_3, dfg$rollingballs_4, dfg$rollingballs_5, dfg$rollingballs_6, dfg$rollingballs_7, dfg$rollingballs_8, dfg$rollingballs_9, dfg$rollingballs_10)
rollingswingstrikes<-list(dfg$rollingswingstrikes_1,dfg$rollingswingstrikes_2, dfg$rollingswingstrikes_3, dfg$rollingswingstrikes_4, dfg$rollingswingstrikes_5, dfg$rollingswingstrikes_6, dfg$rollingswingstrikes_7, dfg$rollingswingstrikes_8, dfg$rollingswingstrikes_9, dfg$rollingswingstrikes_10)

#Extra Base Hits
eb<-paste0('eb',i)
dfg[,eb]<-rollingdoubles[[i]]+rollingtriples[[i]]+rollinghr[[i]]
eb<-list(dfg$eb_1, dfg$eb_2, dfg$eb_3, dfg$eb_4, dfg$eb_5, dfg$eb_6, dfg$eb_7, dfg$eb_8, dfg$eb_9, dfg$eb_10)

#WOBA
woba<-paste0('woba',i)
dfg[,woba]<-(.7*(rollingubb[[i]]+rollingibb[[i]])+.9*rollingsingles[[i]]+1.25*rollingdoubles[[i]]+1.6*rollingtriples[[i]]+2*rollinghr[[i]])/rollingpa[[i]]

#ISO
iso<-paste0('iso',i)
dfg[,iso]<-(rollingdoubles[[i]]+rollingtriples[[i]]+rollinghr[[i]])/rollingab[[i]]

#Strikes to Balls Ratio
stbratio<-paste0('stbratio', i)
dfg[,stbratio]<-rollingswingstrikes[[i]]/rollingballs[[i]]
}

saveRDS(dfg, "C:/Users/Sean/Documents/R_Baseball_Project/Datasets/df7.rds")

df<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/df.rds")
df2<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/df2.rds")
df3<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/df3.rds")
df4<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/df4.rds")
df5<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/df5.rds")
df6<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/df6.rds")
df7<-readRDS("C:/Users/Sean/Documents/R_Baseball_Project/Datasets/df7.rds")

df8<-rbind.fill(df,df2,df3,df4,df5,df6, df7)
#Creating four separate datasets based on matchup
rldf <- df7[which(df7$BAT_HAND_CD=='R' & df7$PIT_HAND_CD=='L'),]
rrdf <-df7[which(df7$BAT_HAND_CD=='R' & df7$PIT_HAND_CD=='R'),]
lldf <- df7[which(df7$BAT_HAND_CD=='L' & df7$PIT_HAND_CD=='L'),]
lrdf <- df7[which(df7$BAT_HAND_CD=='L' & df7$PIT_HAND_CD=='R'),]


saveRDS(rldf, "C:/Users/Sean/Documents/R_Baseball_Project/Datasets/rldf.rds")
saveRDS(rrdf, "C:/Users/Sean/Documents/R_Baseball_Project/Datasets/rrdf.rds")
saveRDS(lldf, "C:/Users/Sean/Documents/R_Baseball_Project/Datasets/lldf.rds")
saveRDS(lrdf, "C:/Users/Sean/Documents/R_Baseball_Project/Datasets/lrdf.rds")




battingstats <- function(a,x,z){
  df$a <- ave(x$EVENT_CD == z, x$bat_id, x$YEAR_ID, FUN=cumsum)
}

df$ubb <- battingstats(ubb,df,14)
df$ibb <- battingstats(ibb,df,15)
df$hbp <- battingstats(hbp,df,16)
df$singles <- battingstats(singles,df,20)
df$doubles <- battingstats(doubles,df,21)
df$triples <- battingstats(triples,df,22)
df$hr <- battingstats(hr,df,23)
df$EB <-battingstats(EB,DF,23|22|21)

#Calculating Righty Lefty Stats
rlstats <- function(a,b){
  df$a <- ave(df$EVENT_CD == b&df$BAT_HAND_CD=="R"&df$PIT_HAND_CD=="L", df$bat_id, df$YEAR_ID, FUN=cumsum)
}

df$rlubb <- rlstats(rlubb,14)
df$rlibb <- rlstats(rlibb,15)
df$rlhbp <- rlstats(rlhbp,16)
df$rlsingles <- rlstats(rlsingles,20)
df$rldoubles <- rlstats(rldoubles,21)
df$rltriples <- rlstats(rltriples,22)
df$rlhr <- rlstats(rlhr,23)
df$rlEB <- df$rldoubles+df$rltriples+df$rlhr
df$rlat_bats <-ave(df$ab_fl=="TRUE"&df$BAT_HAND_CD=="R"&df$PIT_HAND_CD=="L", df$bat_id, df$YEAR_ID, FUN=cumsum)
df$rliso<-df$rlEB/df$rlat_bats

#Calculating Righty Righty Stats
rrstats <- function(a,b){
  df$a <- ave(df$EVENT_CD == b&df$BAT_HAND_CD=="R"&df$PIT_HAND_CD=="R", df$bat_id, df$YEAR_ID, FUN=cumsum)
}

df$rrubb <- rrstats(rrubb,14)
df$rribb <- rrstats(rribb,15)
df$rrhbp <- rrstats(rrhbp,16)
df$rrsingles <- rrstats(rrsingles,20)
df$rrdoubles <- rrstats(rrdoubles,21)
df$rrtriples <- rrstats(rrtriples,22)
df$rrhr <- rrstats(rrhr,23)
df$rrEB <- df$rrdoubles+df$rrtriples+df$rrhr
df$rrat_bats <-ave(df$ab_fl=="TRUE"&df$BAT_HAND_CD=="R"&df$PIT_HAND_CD=="R", df$bat_id, df$YEAR_ID, FUN=cumsum)
df$rriso<-df$rrEB/df$rrat_bats


#Calculating Lefty Righty Stats
lrstats <- function(a,b){
  df$a <- ave(df$EVENT_CD == b&df$BAT_HAND_CD=="L"&df$PIT_HAND_CD=="R", df$bat_id, df$YEAR_ID, FUN=cumsum)
}

df$lrubb <- lrstats(lrubb,14)
df$lribb <- lrstats(lribb,15)
df$lrhbp <- lrstats(lrhbp,16)
df$lrsingles <- lrstats(lrsingles,20)
df$lrdoubles <- lrstats(lrdoubles,21)
df$lrtriples <- lrstats(lrtriples,22)
df$lrhr <- lrstats(lrhr,23)
df$lrEB <- df$lrdoubles+df$lrtriples+df$lrhr
df$lrat_bats <-ave(df$ab_fl=="TRUE"&df$BAT_HAND_CD=="L"&df$PIT_HAND_CD=="R", df$bat_id, df$YEAR_ID, FUN=cumsum)
df$lriso<-df$lrEB/df$lrat_bats


#Calculating Lefty Lefty Stats
llstats <- function(a,b){
  df$a <- ave(df$EVENT_CD == b&df$BAT_HAND_CD=="L"&df$PIT_HAND_CD=="L", df$bat_id, df$YEAR_ID, FUN=cumsum)
}

df$llubb <- llstats(llubb,14)
df$llibb <- llstats(llibb,15)
df$llhbp <- llstats(llhbp,16)
df$llsingles <- llstats(llsingles,20)
df$lldoubles <- llstats(lldoubles,21)
df$lltriples <- llstats(lltriples,22)
df$llhr <- llstats(llhr,23)
df$llEB <- df$lldoubles+df$lltriples+df$llhr
df$llat_bats <-ave(df$ab_fl=="TRUE"&df$BAT_HAND_CD=="L"&df$PIT_HAND_CD=="L", df$bat_id, df$YEAR_ID, FUN=cumsum)
df$lliso<-df$llEB/df$llat_bats


pa <- function(a,b,c){
  df$a <- ave(df$BAT_HAND_CD==b & df$PIT_HAND_CD==c &(df$ab_fl==TRUE|df$EVENT_CD==14|df$EVENT_CD==15|df$EVENT_CD==16), df$bat_id, df$YEAR_ID, FUN=cumsum)
}

df$rlpa <- pa(rlpa,"R", "L")
df$rrpa <- pa(rrpa,"R", "R")
df$lrpa <- pa(lrpa,"L", "R")
df$llpa <- pa(llpa,"L", "L")


#Calculating WOBA statistic by hitting/pitching matchup
df$rlwoba <-(.7*(df$rlubb+df$rlibb)+.9*df$rlsingles+1.25*df$rldoubles+1.6*df$rltriples+2*df$rlhr)/df$rlpa
df$rrwoba <-(.7*(df$rrubb+df$rribb)+.9*df$rrsingles+1.25*df$rrdoubles+1.6*df$rrtriples+2*df$rrhr)/df$rrpa
df$lrwoba <-(.7*(df$lrubb+df$lribb)+.9*df$lrsingles+1.25*df$lrdoubles+1.6*df$lrtriples+2*df$lrhr)/df$lrpa
df$llwoba <-(.7*(df$llubb+df$llibb)+.9*df$llsingles+1.25*df$lldoubles+1.6*df$lltriples+2*df$llhr)/df$llpa


df$battingsplit <- with(df, interaction(df$BAT_HAND_CD, df$PIT_HAND_CD))
df$interaction  <- with(df, interaction(df$battingsplit, df$stadium, df$bat_id))

#Creating four separate datasets based on matchup
rldf <- df[which(df$battingsplit=='R.L'),]
rrdf <-df[which(df$battingsplit=='R.R'),]
lldf <- df[which(df$battingsplit=='L.L'),]
lrdf <- df[which(df$battingsplit=='L.R'),]

#Aggregating at-bat level data into game level data by matchup
matchups<-function(a){
  a2 <- aggregate(list(fantasypoints=a$fantasypoints), by=list(GAME_ID=a$GAME_ID, bat_id=a$bat_id,
                                                               stadium=a$stadium, interaction=a$interaction,
                                                               BAT_HAND_CD=a$BAT_HAND_CD, PIT_HAND_CD=a$PIT_HAND_CD,
                                                               YEAR_ID=a$YEAR_ID), FUN=sum)
}

lldf2<-matchups(lldf)
lrdf2<-matchups(lrdf)
rldf2<-matchups(rldf)
rrdf2<-matchups(rrdf)

#Combining with calcualted WOBA and ISO variables
lldf3 <- aggregate(list(llwoba=lldf$llwoba,lliso=lldf$lliso), by=list(bat_id=lldf$bat_id, GAME_ID=lldf$GAME_ID), FUN=last)
lrdf3 <- aggregate(list(lrwoba=lrdf$lrwoba,lriso=lrdf$lriso), by=list(bat_id=lrdf$bat_id, GAME_ID=lrdf$GAME_ID), FUN=last)
rldf3 <- aggregate(list(rlwoba=rldf$rlwoba,rliso=rldf$rliso), by=list(bat_id=rldf$bat_id, GAME_ID=rldf$GAME_ID), FUN=last)
rrdf3 <- aggregate(list(rrwoba=rrdf$rrwoba,rriso=rrdf$rriso), by=list(bat_id=rrdf$bat_id, GAME_ID=rrdf$GAME_ID), FUN=last)

#Final Matchup Datasets
lldf2<-merge(lldf2, lldf3, by=c("bat_id","GAME_ID"))
lrdf2<-merge(lrdf2, lrdf3, by=c("bat_id","GAME_ID"))
rldf2<-merge(rldf2, rldf3, by=c("bat_id","GAME_ID"))
rrdf2<-merge(rrdf2, rrdf3, by=c("bat_id","GAME_ID"))

indexes = sample(1:nrow(lrdf2), size=0.2*nrow(lrdf2))
lrdf2test<- lrdf2[indexes,]
lrdf2train<- lrdf2[-indexes,]
#####################################################
#####################################################
#             Modeling Starts Here
#####################################################
#####################################################

#Regression Tree
tree.model <- rpart(fantasypoints~stadium + lrwoba + lriso, 
                    method="anova", data=lrdf2)

p<-as.data.frame(predict(tree.model, lrdf2))
cor(p, lrdf2$fantasypoints)

printcp(tree.model)
plotcp(tree.model)

rpart.plot(tree.model, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)

#Model Tree
m5 <- M5P(fantasypoints ~ lriso+lrwoba+stadium, data = lrdf2train)
p<-as.data.frame(predict(m5, lrdf2test))
summary(m5)
plot(m5)
cor(p, lrdf2test$fantasypoints)
acc <- data.frame(cbind(actuals=lrdf2test$fantasypoints, predicteds=p))
acc$predict.m5..lrdf2test.<-ifelse(acc$predict.m5..lrdf2test.<0,0,acc$predict.m5..lrdf2test.) 




