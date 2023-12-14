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

#ESTIMATING RECAPTURE PROBABILITY
#Year by year probability for recapture p in vector form
CapProb = c(0.0492643, 0.0240814, 0.021784, 0.0253501, 0.0305672, 0.0220467, 0.0234167, 0.0180681, 0.0199426, 0.0175239, 0.0182425, 0.023292,
            0.020789, 0.0270455, 0.0201497, 0.0201527, 0.0228601, 0.0236721, 0.0299981, 0.0165172)

#Years in vector form
Time = c(1964:1983)

#two vectors converted to a data frame
Probdata = data.frame(CapProb, Time)

#plot of the recapture probabilities with their respective dates
ggplot(Probdata, aes(Time, CapProb)) + geom_point() +geom_smooth(method = "loess", colour = 'red') + labs(title = "Estimated Recapture Probability of Pulli Blackbirds (CJS)",y="Recapture Probability", x="Year") +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "black")) +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "black")) +
  theme_classic() +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"))

#ESTIMATING SURVIVAL PROBABILITY
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
