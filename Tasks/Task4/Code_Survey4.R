## POLS 318 Survey 4 Data
### November 2020

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
MyData <- read_excel("~/Dropbox/TAMU/POLS318_IR/Tasks/Task4/Task4_Edit.xlsx")

### Experiment 1

# Remove middle category (total 10 respondents)
MyData2 <- with(MyData, MyData[!(approve1 == 4), ])

# Code binary variables (1=approve; 0=disapprove)
MyData2 <- MyData2 %>%
  mutate(support = ifelse(approve1>3, 1, 0))

# Label facets
covert.lab <- c("Overt", "Covert", "Covet-Secret")
names(covert.lab) <- c(1, 2, 3)

## Figure 1: approval (binary) by 3 covert levels
p1 <- ggplot(MyData2, aes(x = factor(support), group = factor(trt_covert1))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.75) +
  geom_text(aes(label = scales::percent(..prop..), fontface = "bold",
                y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_brewer(palette="Set1") +
  scale_x_discrete(labels = c("0" = "Disapprove", "1" = "Appprove")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") + 
  facet_grid(~trt_covert1, labeller = labeller(trt_covert1 = covert.lab)) +
  theme_bw()

p1 <- p1 + theme(legend.position = 'none')

# Figure 2: aggregate choices to Approve/Disapprove

# Remove middle category (total 6 respondents)
MyData2 <- with(MyData, MyData[!(results1 == 4), ])

# Code binary variables (1=approve; 0=disapprove)
MyData2 <- MyData2 %>%
  mutate(support2 = ifelse(results1>3, 1, 0))

# Label facets
result.lab <- c("No Success", "Success")
names(result.lab) <- c(0, 1)

p2 <- ggplot(MyData2, aes(x = factor(support2), group = factor(trt_result1))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.65) +
  geom_text(aes( label = scales::percent(..prop..), fontface = "bold",
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_brewer(palette="Set1") +
  scale_x_discrete(labels = c("0" = "Disapprove", "1" = "Approve")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") +
  facet_grid(trt_result1 ~ trt_covert1, labeller = labeller(trt_result1 = result.lab, trt_covert1 = covert.lab)) +
  theme_bw()

p2 <- p2 + theme(legend.position = 'none')

# Regression models: two DVs
library(modelsummary)

summary(m1 <- lm(approve1 ~ factor(trt_covert1) + gender + poli, data = MyData))
summary(m1a <- lm(results1 ~ factor(trt_covert1) + factor(trt_result1) + gender + poli, data = MyData))

m <- list(m1, m1a)
modelsummary(m, stars = T, coef_rename = c("factor(trt_covert1)2" = "Covert", "factor(trt_covert1)3" = "Covert_Secret",
                                           "factor(trt_result1)1" = "Success", "gender" = "Gender", "poli" = "Party"))

### Experiment 2  ###
# Figure 3: approve/disapprove for covert treatment (7-scale results)

p3 <- ggplot(MyData, aes(x = factor(approve2), group = factor(trt_covert2))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.75) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_x_discrete(labels = c("1" = "Dis.Strong", "2" = "Dis.Some", "3" = "Dis.Slight", "4" = "Neutral",
                              "5" = "App.Slight", "6" = "App.Some", "7" = "App.Stong")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") + 
  facet_grid(~trt_covert2, labeller = labeller(trt_covert2 = covert2.lab)) +
  theme_bw() + coord_flip()

p3 <- p3 + theme(legend.position = 'none')

# Figure 4: binary approve, conditions are covert and policy type
# Remove middle category (total 5 respondents)
MyData3 <- with(MyData, MyData[!(approve2 == 4), ])

# Code binary variables (1=approve; 0=disapprove)
MyData3 <- MyData3 %>%
  mutate(support3 = ifelse(approve2>3, 1, 0))

# label facets
covert2.lab <- c("Overt action", "Covert action")
names(covert2.lab) <- c(0, 1)
pol.lab <- c("Policy: Finance", "Policy: Force")
names(pol.lab) <- c(0, 1)

p4 <- ggplot(MyData3, aes(x = factor(support3), group = factor(trt_covert2))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.75) +
  geom_text(aes(label = scales::percent(..prop..), fontface = "bold",
                y= ..prop.. ), stat= "count", vjust = -.5) +
    scale_fill_brewer(palette="Set1") +
    scale_x_discrete(labels = c("0" = "Disapprove", "1" = "Appprove")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") + 
  facet_grid(trt_pol ~ trt_covert2, labeller = labeller(trt_pol = pol.lab, trt_covert2 = covert2.lab)) +
  theme_bw()

p4 <- p4 + theme(legend.position = 'none')

### Experiment 3  ###

# Figure 5: approve (7 values) for info or not on approval (across covert types)
# label facets
opp.lab <- c("No Info", "Public Oppose")
names(opp.lab) <- c(0, 1)

p5 <- ggplot(MyData, aes(x = factor(approve3), group = factor(trt_opp))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.75) +
  geom_text(aes(label = scales::percent(..prop..), fontface = "bold",
                y= ..prop.. ), stat= "count", hjust = 0) +
    scale_x_discrete(labels = c("1" = "Dis.Strong", "2" = "Dis.Some", "3" = "Dis.Slight", "4" = "Neutral",
                                "5" = "App.Slight", "6" = "App.Some", "7" = "App.Stong")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") + ylim(0, 0.4) +
  facet_grid(~trt_opp, labeller = labeller(trt_opp = opp.lab)) +
  coord_flip() +
  theme_bw()

p5 <- p5 + theme(legend.position = 'none')

# Figure 6: binary approve, conditions are covert and public oppose
# Remove middle category (total 9 respondents)
MyData4 <- with(MyData, MyData[!(approve3 == 4), ])

# Code binary variables (1=approve; 0=disapprove)
MyData4 <- MyData4 %>%
  mutate(support4 = ifelse(approve3>3, 1, 0))

ggplot(MyData, aes(x = factor(results3), group = factor(trt_results3))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.75) +
  geom_text(aes(label = scales::percent(..prop..), fontface = "bold",
                y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_x_discrete(labels = c("1" = "Dis.Strong", "2" = "Dis.Some", "3" = "Dis.Slight", "4" = "Neutral",
                              "5" = "App.Slight", "6" = "App.Some", "7" = "App.Stong")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") + 
  facet_grid(~trt_results3) +
  theme_bw()

p6 <- ggplot(MyData4, aes(x = factor(support4), group = factor(trt_covert3))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..)), stat = "count", width = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), fontface = "bold",
                y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_brewer(palette="Set1") +
  scale_x_discrete(labels = c("0" = "Disapprove", "1" = "Appprove")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") + 
  facet_grid(trt_opp ~ trt_covert3, labeller = labeller(trt_opp = opp.lab, trt_covert3 = covert2.lab)) +
  theme_bw()

p6 <- p6 + theme(legend.position = 'none')

## Other measures
# Transparency distribution

p7 <- ggplot(MyData, aes(x = factor(transparent_imp))) +
  geom_bar(aes(y = ..prop..,  fill = factor(..x..), group = 1), stat = "count", width = 0.75) +
  geom_text(aes(label = scales::percent(..prop..), fontface = "bold", group = 1,
                y= ..prop.. ), stat= "count", hjust = 0) +
  scale_x_discrete(labels = c("1" = "Not too Imp.", "5" = "Very Important")) +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("") + ylab("Proportion") + 
  coord_flip() +
  theme_bw()

p7 <- p7 + theme(legend.position = 'none')

# Trust cont. measure (no interactions): can be added to Git file
library(ggeffects)

summary(m <- glm(support4 ~ factor(trt_covert3) + 
                  trust_govt + transparent_imp + gender + poli, data = MyData4, family = binomial(link = "probit")))

pre <- ggpredict(m, terms = c("trt_covert3", "trust_govt [17, 30, 56, 75]"))
p8 <- plot(pre, ci.style = "errorbar") +
  labs(
    x = "",
    y = "Probability of support",
    title = "Predicted probability of public support for intervention policy",
    colour = "Degree of trust in government") +
  scale_x_continuous(labels = c("Overt", "Covert"), breaks = c(0, 1)) +
  scale_colour_brewer(palette = "Set1", labels = c("10th", "25th", "75th", "90th")) +
  theme(legend.position = "bottom")


# Interactions of policy type and outcomes conditions
## Experiment 2 

MyData5 <- describeBy(MyData$results2,list(MyData$trt_covert2,MyData$trt_results2), mat=TRUE,digits=2)

names(MyData5)[names(MyData5) == 'group1'] = 'type'
names(MyData5)[names(MyData5) == 'group2'] = 'outcomes'

levels(MyData5$type)[levels(MyData5$type)=='0'] = 'Overt'
levels(MyData5$type)[levels(MyData5$type)=='1'] = 'Covert'

levels(MyData5$outcomes)[levels(MyData5$outcomes)=='0'] = 'Unsuccessful'
levels(MyData5$outcomes)[levels(MyData5$outcomes)=='1'] = 'Successful'

MyData5$se <- MyData5$sd/sqrt(MyData5$n)

limits = aes(ymax = mean + se, ymin=mean - se)
dodge = position_dodge(width=0.9)
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'))

p9a <- ggplot(MyData5, aes(x = outcomes, y = mean, fill = type))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.1)+
  apatheme + scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(labels = c("0" = "Unsuccessful", "1" = "Successful")) +
  ylab('Mean Approval') + xlab("Policy Outcomes") + ggtitle("Experiment #2") +
  scale_fill_manual(values = c("blue", "red"),
                    name = "Policy Type",
                    breaks = c(0, 1),
                    labels = c("Overt", "Covert")) 
  
  
## Experiment 3 

MyData6 <- describeBy(MyData$results3,list(MyData$trt_covert3,MyData$trt_results3), mat=TRUE,digits=2)

names(MyData6)[names(MyData6) == 'group1'] = 'type'
names(MyData6)[names(MyData6) == 'group2'] = 'outcomes'

levels(MyData6$type)[levels(MyData6$type)=='0'] = 'Overt'
levels(MyData6$type)[levels(MyData6$type)=='1'] = 'Covert'

levels(MyData6$outcomes)[levels(MyData6$outcomes)=='0'] = 'Unsuccessful'
levels(MyData6$outcomes)[levels(MyData6$outcomes)=='1'] = 'Successful'

MyData6$se <- MyData6$sd/sqrt(MyData6$n)

limits = aes(ymax = mean + se, ymin=mean - se)
dodge = position_dodge(width=0.9)
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'))

p9b <- ggplot(MyData6, aes(x = outcomes, y = mean, fill = type))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.1)+
  apatheme + scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(labels = c("0" = "Unsuccessful", "1" = "Successful")) +
  ylab("") + xlab("Policy outcomes") + ggtitle("Experiment #3") +
  scale_fill_manual(values = c("blue", "red"),
                    name = "Policy Type",
                    breaks = c(0, 1),
                    labels = c("Overt", "Covert")) 

p9 <- ggarrange(p9a, p9b, nrow = 1, ncol = 2, common.legend = T, legend = "bottom")
p9 <- annotate_figure(p9,
                top = text_grob("Public support for foreign intervention",
                                color = "black",
                                face = "bold",
                                size = 16))


#################### Coding for data  ####################
### Scenario 1: Dictator (3 levels of cover)
# trt_covert1: 1=overt; 2=covert; 3=covert (secret).
# trt_results1: 0= no success (fail); 1=success.

### Scenario 2: Civil war (cover and policy type)
# trt_covert2: 0=overt; 1=covert.
# trt_pol: 0=money; 1=military force.
# trt_results2: 0= no success (fail); 1=success

### Scenario 3: Dictator (cover and info on public reject)
# trt_covert3: 0=overt; 1=covert.
# trt_opp: 0=no info; 1=info on opposition.
# trt_results3: 0= no success (fail); 1=success

# Approval (all scenarios and results): disapprove strong (1); disapprove somewhat (2); disapprove slightly (3); neutral (4); 
#                                       approve slightly (5); approve somewhat (6); approve strong (7)

# gender (male=0; female=1)
# Transparency: not too important (1) ==> very important (5)
# Trust government: 0 (hardly ever) ==> 100 (just about always).
# Support covert action: 0=no; 1=yes.

