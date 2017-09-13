
library(dplyr)
##install.packages("circlize")
library(circlize)
load("Event_History.Rdata")

Preprocess <- function(pc.specific, event.history = Event.History) {
  event.history[["pc.specific"]] <- event.history[["DUPERSID"]] %in% pc.specific[["DUPERSID"]]
  pc.specific <- subset(event.history, event.history[["pc.specific"]])
  pc.specific <- pc.specific[c("DUPERSID", "date", "TYPE")]
  start.wait <- distinct(pc.specific, DUPERSID)
  
  start.wait <- mutate(start.wait, date = as.Date(c("2011-01-01")), TYPE = 0)
  end.wait <- mutate(start.wait, date = as.Date(c("2011-12-31")), TYPE = 1)
  pc.specific <- rbind(pc.specific, end.wait, start.wait)
  result <- na.omit(pc.specific[order(pc.specific[["DUPERSID"]], pc.specific[["date"]], pc.specific[["TYPE"]]), ])
  
  return(result)
}

Reform <- function(df, type, date) {
  dif.date <- vector("integer", length(df[[date]]))
  date2 <- c(NaN, df[[date]])
  dif.date <- c(df[[date]],NaN) - date2
  dif.date <- dif.date[-length(date2)]
  type2<- c(df[[type]],10)
  type2 <- type2[-1]
  type <- df[[type]]
  result <- as.data.frame(cbind(dif.date, type, type2))
  return(result)
}

TransitionMatrix <- function(dif.date, TYPE, TYPE2) {
  # Form a transition matrix of a markov chain which contants 9 states
  # 1-ending 2-obe 3-ope 4-ere 5-hip 6-postobe 7-postope 8-postere 9-posthip
  #
  # Args:
  #   TYPE: 
  #   date: 
  #
  # Return:
  #   
  # 
  cmatr <- matrix(0, 9, 9)
  rownames(cmatr) <- c("f.ending", "f.obe", "f.ope", "f.ere", "f.hip", "f.postobe", "f.postope", "f.postere", "f.posthip")
  colnames(cmatr) <- c("t.ending", "t.obe", "t.ope", "t.ere", "t.hip", "t.postobe", "t.postope", "t.postere", "t.posthip")
##   dif.date <- vector("integer", length(date))
##   date2 <- c(NaN, date)
##   dif.date <- c(date,NaN) - date2
##   dif.date <- dif.date[-length(date2)]
##   TYPE2 <- c(TYPE,10)
##   TYPE2 <- TYPE2[-1]
  
  for (a in 2:5)  {
    # From event to post event period
    cmatr[a, a+4] <- sum(TYPE == a) 
    # From post event to post event, dif.date times
    cmatr[a+4, a+4] <- sum(as.numeric(dif.date[TYPE == a & dif.date > 1]),na.rm = TRUE)
    for(b in 1:5)   {
      # From post event period to event
      cmatr[a+4, b] <- sum(TYPE == a & TYPE2 == b & dif.date > 2, na.rm = TRUE)
      # From event to straightly to another event
      cmatr[a, b] <- sum(TYPE == a & TYPE2 == b & (dif.date <= 2 | dif.date >= 0), na.rm = TRUE)
    }
  }
  tmatr <- prop.table(cmatr, 1)
  return(tmatr)
}

hib.yes <- subset(priority.condition, priority.condition$HIBPDXY1 == 1 | priority.condition$HIBPDXY2 == 1)
hib.no <- subset(priority.condition, priority.condition$HIBPDXY1 == 2 & priority.condition$HIBPDXY2 == 2)
ohib.yes <- Preprocess(hib.yes)
ohib.no <- Preprocess(hib.no)
hib.yes <- Reform(ohib.yes,"TYPE", "date")
hib.no <- Reform(ohib.no, "TYPE", "date")
hib.yes.transmatrix <- TransitionMatrix(hib.yes$dif.date, hib.yes$type, hib.yes$type2)  
hib.no.transmatrix <- TransitionMatrix(hib.no$dif.date, hib.no$type, hib.no$type2)


hib.yes.chord <- chordDiagram(hib.yes.transmatrix)
hib.no.chord <- chordDiagram(hib.no.transmatrix)

