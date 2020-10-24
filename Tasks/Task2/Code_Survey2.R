## Data and analysis for survey 2
### October 2020

library(psych)  
library(foreign)
library(ggplot2)
library(gplots)
library(MASS)
library(Hmisc)
library(ggthemes)
library(devtools)
library(gridExtra)
library(dplyr)
library(ggpubr)

# Upload Data
library(readxl)
MyData <- read_excel("~/")

# Code binary variables (0=favor attack; 1=oppose attack)
MyData$support[MyData$choice>3] <- 1
MyData$support[MyData$choice<3] <- 0
MyData$support2[MyData$choice2>3] <- 1
MyData$support2[MyData$choice2<3] <- 0

# Label facets for conditions
reg.lab <- c("Adversary regime: Nondemocracy", "Adversary regime: Democracy")
names(reg.lab) <- c(0, 1)

trade.lab <- c("No Trade with US", "Trade with US")
names(trade.lab) <- c(0,1)

### Figure 1: all choice options

ggplot(MyData, aes(x = factor(choice), group = factor(regime_trt))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels = c("1" = "Fav.Strong", "2" = "Fav.Some", "3" = "Neither",
                              "4" = "Opp.Some", "5" = "Opp.Strong")) +
  xlab("") + ylab("Proportion") + 
  facet_grid(~regime_trt, labeller = labeller(regime_trt = reg.lab)) +
  theme_bw()

### Figure 2: aggregate choices to favor/oppose (regime type condition)

# Remove middle category
MyData2 <- with(MyData, MyData[!is.na(support), ])

p2<- ggplot(MyData2, aes(x = factor(support), group = factor(regime_trt))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.75) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_brewer(palette="Set1") +
  scale_x_discrete(labels = c("0" = "Favor attack", "1" = "Oppose attack")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") + ggtitle("Experiment 1") +
  facet_grid(~regime_trt, labeller = labeller(regime_trt = reg.lab)) +
  theme_bw()

p2 <- p2 + theme(legend.position = 'none')

### Figure 3: aggregate choices to favor/oppose (trade with US condition)

p3<- ggplot(MyData2, aes(x = factor(support), group = factor(trade_trt))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.75) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_brewer(palette="Set2") +
  scale_x_discrete(labels = c("0" = "Favor attack", "1" = "Oppose attack")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") +
  facet_grid(~trade_trt, labeller = labeller(trade_trt = trade.lab)) +
  theme_bw()

p3 <- p3 + theme(legend.position = 'none')

## Experiment 2, remove missing data
MyData3 <- with(MyData, MyData[!is.na(support2), ])

### Figure 4: all choice options
p4 <- ggplot(MyData, aes(x = factor(choice2), group = factor(regime_trt3))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.65) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels = c("1" = "App.Strong", "2" = "App.Some", "3" = "Neither",
                              "4" = "Disapp.Some", "5" = "Disapp.Strong")) +
  xlab("") + ylab("Proportion") +
  facet_grid(~regime_trt3, labeller = labeller(regime_trt3 = reg.lab)) +
  theme_bw()

p4 <- p4 + theme(legend.position = 'none')


### Figure 5: aggregate choices to favor/oppose (regime type condition)
p5 <- ggplot(MyData3, aes(x = factor(support2), group = factor(regime_trt3))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.75) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_brewer(palette="Set1") +
  scale_x_discrete(labels = c("0" = "Favor attack", "1" = "Oppose attack")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("") + ggtitle("Experiment 2") +
  facet_grid(~regime_trt3, labeller = labeller(regime_trt3 = reg.lab)) +
  theme_bw()


p5 <- p5 + theme(legend.position = 'none') 

pl <- ggarrange(p2, p5, ncol = 2, nrow = 1)

#################### Coding for data  ####################
# trt1: regime (dictator=0; democracy=1)
# trt2: trade (low=0; high=1)
# trt3: regime2 (dictator=0; democracy=1)
# trt4: ally (no=0; yes=1)
# choice: favor strongly (1); favor somewhat (2); neither (3); oppose somewhat (4); oppose strongly (5)
# moral (no=0; yes=1)
# gender (male=0; female=1)



