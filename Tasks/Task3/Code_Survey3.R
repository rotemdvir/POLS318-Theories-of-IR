## POLS 318 Survey 3 Data
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

# Set Randomizer
set.seed(2020)

# Upload Data
library(readxl)
MyData <- read_excel("~/Dropbox/TAMU/POLS318_IR/Tasks/Task3/Task3_Edit.xlsx")

# Experiment 1

# Code binary variables (0=approve; 1=disapprove)
MyData <- MyData %>%
  mutate(support = ifelse(choice>3, 1, 0))

# Label facets
pol.lab <- c("Stay Out", "Empty Threat")
names(pol.lab) <- c(0, 1)

## Figure 1: all response options 
p1 <- ggplot(MyData, aes(x = factor(choice), group = factor(trt_pol))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.75) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_x_discrete(labels = c("1" = "App.Strong", "2" = "App.Some", "3" = "Neither",
                              "4" = "Disapp.Some", "5" = "Disapp.Strong")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") + ylim(0, 0.45) +
  facet_grid(~trt_pol, labeller = labeller(trt_pol = pol.lab)) +
  theme_bw()

p1 <- p1 + theme(legend.position = 'none')

# Figure 2: aggregate choices to Approve/Disapprove

# Remove middle category (total 13 respondents)
MyData2 <- with(MyData, MyData[!(choice == 3), ])

p2 <- ggplot(MyData2, aes(x = factor(support), group = factor(trt_pol))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.65) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_brewer(palette="Set1") +
  scale_x_discrete(labels = c("0" = "Approve", "1" = "Disapprove")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") +
  facet_grid(~trt_pol, labeller = labeller(trt_pol = pol.lab)) +
  theme_bw()

p2 <- p2 + theme(legend.position = 'none')

# Figure 3: Density plots of continuous measure by group (stay out/empty threat)
# add mean by group
library(grid)

means <- ddply(MyData, "trt_pol", summarize, grp.m = mean(choice_cont))

p3 <- ggplot(MyData, aes(x = choice_cont, fill = factor(trt_pol), color = factor(trt_pol))) +
  geom_histogram(alpha=0.5, position="identity", binwidth = 10) +
  geom_vline(data = means, aes(xintercept = grp.m, color = factor(trt_pol)), linetype = "dashed", size = 1.2) +
  geom_text(x = 12, y = 7, label = "Stay Out", color = "blue") +
  geom_text(x = 75, y = 7, label = "Empty Threat", color = "red") +
  scale_fill_brewer(palette="Set1") +
  xlab("Approve - Disapprove scale") + ggtitle("Degree of approval - continous scale") + 
  theme_bw() + theme(legend.position = "none")

  
# Crosstabs of disapproval by military condition
dat1 <- MyData %>%
        filter(trt_mil == 0) 

dat2 <- MyData %>%
  filter(trt_mil == 1) 

table(dat1$trt_pol, dat1$choice) # weak military(empty = 81% - stay-out = 41% = 40% AC)
table(dat2$trt_pol, dat2$choice) # strong military(empty = 68% - stay-out = 52% = 16% AC)

## Experiment 2

# Code binary variables (0=approve; 1=disapprove)
MyData <- MyData %>%
  mutate(support2 = ifelse(choice2>3, 1, 0))

# Remove middle category (total 13 respondents)
MyData3 <- with(MyData, MyData[!(choice2 == 3), ])

pol.lab2 <- c("Backed In", "Empty Threat")
names(pol.lab2) <- c(0, 1)
info.lab <- c("No Info", "New info")
names(info.lab) <- c(0, 1)

# Figure 4: all response options
p4 <- ggplot(MyData, aes(x = factor(choice2), group = factor(trt2_incon))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.75) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_x_discrete(labels = c("1" = "App.Strong", "2" = "App.Some", "3" = "Neither",
                              "4" = "Disapp.Some", "5" = "Disapp.Strong")) +
  scale_fill_brewer(palette="Set1") +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") +
  facet_grid(~trt2_incon, labeller = labeller(trt2_incon = pol.lab2)) +
  theme_bw()

p4 <- p4 + theme(legend.position = 'none')

# Figure 5: aggregate choices to Approve/Disapprove

p5 <- ggplot(MyData3, aes(x = factor(support2), group = factor(trt2_incon))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.75) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_brewer(palette="Set1") +
  scale_x_discrete(labels = c("0" = "Approve", "1" = "Disapprove")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("") +
  facet_grid(. ~ trt2_incon + trt2_info, labeller = labeller(trt2_incon = pol.lab2, trt2_info = info.lab)) +
  theme_bw()

p5 <- p5 + theme(legend.position = 'none')

# Crosstabs of disapproval by policy type (back-in, back-down) 
dat3 <- MyData %>%
  filter(trt2_incon == 0)

dat4 <- MyData %>%
  filter(trt2_incon == 1)

# Display results for reputation, competence anc credibility items
table(dat3$rep)
table(dat4$rep)

table(dat3$comp)
table(dat4$comp)

table(dat3$credible)
table(dat4$credible)

# Add results to dataset (proportion of bad reputation, credibility and incompetnece)
dat5 <- tibble(
  pol = c("Backing-In", "Back-Down"),
  reputation = c(0.45, 0.9),
  credibility = c(0.51, 0.9),
  incompetent = c(0.3, 0.68))

p6a <- ggplot(dat5, aes(x = factor(pol), y = reputation)) +
  geom_bar(fill = "darkblue", stat = "identity", width = 0.5) +
  geom_text(aes(label = scales::percent(reputation)), vjust = -0.3) +
  xlab("") + ylab("Damage to Reputation") +
  theme_bw()

p6b <- ggplot(dat5, aes(x = factor(pol), y = credibility)) +
  geom_bar(fill = "maroon", stat = "identity", width = 0.5) +
  geom_text(aes(label = scales::percent(credibility)), vjust = -0.3) +
  xlab("") + ylab("US Credibility Damage") +
  theme_bw()

p6c <- ggplot(dat5, aes(x = factor(pol), y = incompetent)) +
  geom_bar(fill = "darkgreen", stat = "identity", width = 0.5) +
  geom_text(aes(label = scales::percent(incompetent)), vjust = -0.3) +
  xlab("") + ylab("President Incompetence") +
  theme_bw() + ylim(0, 0.9)

# Plot negative effects of breaking promises

p6 <- ggarrange(p6a, p6b, p6c,
          ncol = 3, nrow = 1)
p6 <-  annotate_figure(p6, top = text_grob("Breaking promises and president approval", color = "darkblue", face = "bold", size = 14))

#################### Coding for data  ####################
### Exp. 1
# trt_pol: stay out=0; empty threat=1
# trt_mil: weak=0; strong=1

### Exp. 2
# trt_pol: stay out=0; empty threat=1
# trt_threat: no=0; yes=1
# trt_info: no=0; yes=1
# trt_incon: back-in=0; backdown=1
# competence, rep and credibility: compare info/not within each policy (separate back-in from back-down) 

# choice: strong approve (1); somewhat approve (2); neither (3); somewhat disapprove (4); strongly disapprove (5)
# gender (male=0; female=1)



