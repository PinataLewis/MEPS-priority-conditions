
library(readxl)
library(dplyr)

Panel15_18 <- read_excel("C:/Users/Yanqing/Downloads/Panel15-18.xlsx", sheet = 4)
names(Panel15_18)
priority.conditions <- c(
  "DUPERSID", "PANEL", "DRSPLTY", "year", "years", "HIBPDXY1", "HIBPDXY2", "HIBPAGY1", 
  "HIBPAGY2", "CHDDXY1",  "CHDDXY2", 
  "CHDAGY1",  "CHDAGY2",  "ANGIDXY1", 
  "ANGIDXY2", "ANGIAGY1", "ANGIAGY2", 
  "MIDXY1",  "MIDXY2",   "MIAGY1",  
  "MIAGY2", "OHRTDXY1", "OHRTDXY2", 
  "OHRTAGY1", "OHRTAGY2", "STRKDXY1", 
  "STRKDXY2", "STRKAGY1", "STRKAGY2", 
  "EMPHDXY1", "EMPHDXY2", "EMPHAGY1", 
  "EMPHAGY2", "CHBRON1",  "CHBRON3", 
  "CHOLDXY1", "CHOLDXY2", "CHOLAGY1", 
  "CHOLAGY2", "CANCERY1", "CANCERY2", 
  "CABLADY1", "CABLADY2", "BLDRAGY1", 
  "BLDRRMY1", "CABRAIY1", "BRAIAGY1", 
  "BRAIRMY1", "CABREAY1", "CABREAY2", 
  "BRSTAGY1", "BRSTRMY1", "CACERVY1", 
  "CACERVY2", "CERVAGY1", "CERVRMY1", 
  "CACOLOY1", "CACOLOY2", "COLOAGY1", 
  "COLORMY1", "CALEUKY1", "LEUKAGY1", 
  "LEUKRMY1", "CALUNGY1", "CALUNGY2", 
  "LUNGAGY1", "LUNGRMY1", "CALYMPY1", 
  "CALYMPY2", "LYMPAGY1", "LYMPRMY1", 
  "CAMELAY1", "CAMELAY2", "MELAAGY1", 
  "MELARMY1", "CAOTHEY1", "CAOTHEY2", 
  "OTHRAGY1", "OTHRRMY1", "CAPROSY1", 
  "CAPROSY2", "PRSTAGY1", "PRSTRMY1", 
  "CASKNMY1", "CASKNMY2", "SKNMAGY1", 
  "SKNMRMY1", "CASKDKY1", "CASKDKY2", 
  "SKDKAGY1", "SKDKRMY1", "CATHROY1", 
  "THRTAGY1", "THRTRMY1", "CATHYRY1", 
  "THYRAGY1", "THYRRMY1", "CAUTERY2", 
  "DIABDXY1", "DIABDXY2", "DIABAGY2", 
  "JTPAIN3",  "JTPAIN5",  "ARTHDXY1", 
  "ARTHDXY2", "ARTHTPY1", "ARTHTPY2", 
  "ARTHAGY1", "ARTHAGY2", "ASTHDXY1", 
  "ASTHDXY2", "ASTHAGY1", "ASTHAGY2", 
  "ASSTIL1",  "ASSTIL3",  "ASSTIL5", 
  "ASATAK1",  "ASATAK3",  "ASATAK5", 
  "ASTHEP1",  "ASTHEP3",  "ASTHEP5", 
  "ASACUT3",  "ASACUT5",  "ASMRCN3", 
  "ASMRCN5",  "ASPREV3",  "ASPREV5", 
  "ASDALY3",  "ASDALY5",  "ASPKFL3", 
  "ASPKFL5",  "ASEVFL3",  "ASEVFL5", 
  "ASWNFL3",  "ASWNFL5",  "ADHDADY1", 
  "ADHDADY2", "ADHDAGY1", "ADHDAGY2")

priority.condition <- Panel15_18[priority.conditions] 

Characteristic <- read_excel("c:/users/yanqing/downloads/MEPS -2011 - Characteristics+Events (1).xlsx", sheet = 13)
obe <- read_excel("c:/users/yanqing/downloads/MEPS -2011 - Characteristics+Events (1).xlsx", sheet = 14)
ope <- read_excel("c:/users/yanqing/downloads/MEPS -2011 - Characteristics+Events (1).xlsx", sheet = 15)
ere <- read_excel("c:/users/yanqing/downloads/MEPS -2011 - Characteristics+Events (1).xlsx", sheet = 16)
hie <- read_excel("c:/users/yanqing/downloads/MEPS -2011 - Characteristics+Events (1).xlsx", sheet = 17)


# Merge year, month, day into date with the assumption that missing dates are all MCAR
ere <- within(ere, {date <- paste(ere$ERDATEYR, ere$ERDATEMM, ere$ERDATEDD, sep = "-")
date[ERDATEYR <= 0|ERDATEMM <= 0|ERDATEDD <= 0] <- NA})
hie <- within(hie, {date <- paste(hie$IPBEGYR, hie$IPBEGMM, hie$IPBEGDD, sep = "-")
date[IPBEGYR <= 0 | IPBEGMM <= 0 | IPBEGDD<=0] <- NA})
obe <- within(obe, {date <- paste(obe$OBDATEYR, obe$OBDATEMM, obe$OBDATEDD, sep = "-")
date[OBDATEYR <= 0 | OBDATEMM <= 0 | OBDATEDD <= 0] <- NA})
ope <- within(ope, {date <- paste(ope$OPDATEYR, ope$OPDATEMM, ope$OPDATEDD, sep = "-")
date[OPDATEYR <= 0 | OPDATEMM <= 0 | OPDATEDD <= 0] <- NA})

# Cast a dataframe that contants all event records
Event.History <-  merge.data.frame(Characteristic, obe, by = "DUPERSID", all = TRUE, sort = TRUE)
Event.History <-  merge.data.frame(Event.History, ope[-1], all = TRUE, sort = TRUE)
Event.History <-  merge.data.frame(Event.History, ere[-1], all = TRUE, sort = TRUE)
Event.History <-  merge.data.frame(Event.History, hie[-1], all = TRUE, sort = TRUE)

# Get rid of characteristic data as well as "missing type" records
Event.History <- subset(Event.History, !is.na(TYPE))  
# Change classes(maybe optional)
Event.History$date <- as.Date(Event.History$date)

rm(Characteristic, ere, hie, obe, ope, priority.conditions)
save(Event.History, priority.condition, file = "Event_History.rdata")