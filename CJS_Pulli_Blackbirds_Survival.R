library(tidyverse)
library(RMark)
library(ggplot2)
BlackbirdData = read_csv('PulliBlackbirds.csv')
head(BlackbirdData)
#basic CJS model for data
BlackbirdModel = mark(BlackbirdData)

#input for CJS annually
Phidot=list(formula=~1)
Phitime=list(formula=~time)
#outfput for CJS annually
pdot=list(formula=~1)
ptime=list(formula=~time)

#CJS with phi and p estimates year by year
BBird.phiage.ptime = mark(BlackbirdData, model.parameters = list(Phi=Phitime, p = ptime))

uclPhi = c(BBird.phiage.ptime[["results"]][["real"]][["ucl"]])[1:20]

lclPhi = c(BBird.phiage.ptime[["results"]][["real"]][["lcl"]])[1:20]

#Years in vector form
Time = c(1964:1983)

SurvProb = c(0.4242461, 0.7967605, 0.6594785, 0.4539874, 0.6307722, 0.6592124, 0.5003944,
             0.6641432, 0.6407954, 0.7042543, 0.6393813, 0.5008579, 0.6691368, 0.4897628, 0.6534945, 0.5683877, 0.5414653, 0.5548245, 0.3939144, 0.9999999)

Survdata = data.frame(SurvProb, Time)

ggplot(Survdata, aes(Time, SurvProb)) + geom_point() + geom_errorbar(aes(ymax = uclPhi, ymin = lclPhi)) + labs(title = "Estimated Survival Rate of Pulli Blackbirds (CJS)",y="Survival Rate", x="Year") +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "black")) +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "black")) +
  theme_classic() +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"))
