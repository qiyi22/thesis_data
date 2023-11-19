library(lme4)
library(jtools)
library(ggplot2)
library(nlme)
library(car)

input <- read.csv("new data raw.csv")
head(input)
# Run a shapiro test to check normal distribution
shapiro.test(input$log_duration)
# Run a Whitney Mann test to compare the reading time from two groups
wilcox.test(c_logduration ~ proficiency, data = input)

control_params <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 30000))

# Check necessity of using multilevel modeling
baseline<-gls(c_log_duration~1, data=input,method="ML",na.action=na.exclude)
M0<-lme(c_log_duration~1, data=input, random= ~1|Item, method="ML",na.action=na.exclude)
anova(baseline,M0)

# Model 1: how collocation frequency, level, Log Dice influence reading time
Model_1 <-lmer(c_logduration~c_logdice+c_zpf_total+level+c_pho_len+(level|Item)+(1|name),data=input,REML=FALSE,na.action=na.exclude,control=control_params)
vif(M1)
summ(Model_1,confint = TRUE, digits = 2)

# Model 1.2: how frequency interacts with proficiency
Model_1.2 <-lmer(c_logduration~c_zpf_total+proficiency+c_zpf_total*proficiency+(proficiency|Item)+(1|name),data=input,REML=FALSE,na.action=na.exclude,control=control_params)
vif(Model_1.2)
summary(Model_1.2)

# Model 1.3: how Log Dice interacts with proficiency
Model_1.3<- lmer(c_logduration~c_logdice+proficiency+c_logdice*proficiency+(proficiency|Item)+(1|name),data=input,REML=FALSE,na.action=na.exclude,control=control_params)
vif(Model_1.3)
summ(Model_1.3,confint = TRUE, digits = 2)

#Model 2: how word frequency, level, Log Dice influence reading time
Model2<-lmer(c_logduration~c_logdice+c_zpf_w1+c_zpf_w2+c_pho_len+level+(level|Item)+(1|name),data=input,REML=FALSE,na.action=na.exclude,control=control_params)
vif(Model2)
summ(Model2,confint = TRUE, digits = 2)

