####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
#######Does Exposure to Evidence on the Biological Sources of Political Attitudes Depolarize? Evidence from a Survey Experiment#####
####################################################Severson (2019)#################################################################
######################################### Code Last Updated: 9/12/2019 ##############################################################
####################################################################################################################################
####################################################################################################################################

###########################################################
###########################################################
###########################################################
######## Clear Workspace + Install/Load Packages ##########
###########################################################
###########################################################
###########################################################

#Clear Global Environment (Uncomment Packages to Install)

rm(list=ls())
while (!is.null(dev.list()))  dev.off()
options(scipen=999)
options(warn=-1)


#install.packages("plyr")
#install.packages("devtools", dependencies=T)
#install.packages("foreign")
#install.packages("Hmisc")
#install.packages("psych")
#install.packages("MASS")
#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("stargazer")
#install.packages("lme4")
#install.packages("readr")
#install.packages("tibble")
#install.packages("dplyr")
#devtools::install_github("ropensci/qualtRics")
#install.packages("qualtRics")
#install.packages("tidyr")
#install.packages("interplot")
#install.packages("mediation")
#install.packages("stringr")
#install.packages("expss")
#install.packages("sjPlot")
#install.packages("sjmisc")
#install.packages("forcats")
#install.packages("scales")
#install.packages("sjlabelled")
#install.packages("sjmisc")
#install.packages("rlang")
#install.packages("ggsci")

library(plyr)
library(devtools)
library(foreign)
library(Hmisc)
library(psych)
library(MASS)
library(ggplot2)
library(stargazer)
library(ggthemes)
library(lme4)
library(readr) 
library(tibble) 
library(dplyr)
library(tidyr)
library(qualtRics)
library(interplot)
library(stringr)
library(mediation)
library(expss) #Helps with Variable Labels
library(sjPlot)
library(sjmisc)
library(forcats)
library(scales)
library(sjlabelled)
library(sjmisc) # for frq()-function
library(rlang)
library(ggsci)
library(gridExtra)

###########################################################
###########################################################
###########################################################
######## Set Working Directory and Download Data ##########
###########################################################
###########################################################
###########################################################

#Pro-Tip: Remember to select options ".csv" + "Download All Fields" + "Use Numeric Values" (Not Default of Choice Text) + "Remove Line Breaks" from Qualtrics to Help Conversion into R Dataframe

setwd("C:/Users/alexseverson/Desktop/Genopolitics Paper/Data/Data")
#setwd("C:/Users/awseverson/Desktop/Genopolitics/Data/Data") 

#Generate Dataframe (df) with sample data read in from .csv file

df <- readSurvey("mturk_final.csv") #Reads in CSV (Eliminates Unnecessary Header Rows of Qualtrics)
tibble::glimpse(df) #Use Glimpse Function from tibble library to Check Out Dataset

#Dataframes by Condition

names(df)<-str_replace_all(names(df), c(" " = "." , "," = "" )) #Helps Deal With Variable Names Coded with Weird

#Generate Treatment Variables

df$treat <- 0
df$treat[df$treatment_Click.Count >= 0] <-1
describe(df$treat)
ggplot(df, aes(x=treat)) + geom_bar() + labs(title="Treatment Status") 
ggplot(treated, aes(x=ideo_gene_percent)) + geom_bar() + labs(title="Treatment Status") 
df$treat_char<- as.character(df$treat)

#Drop Attention Check Failures

df1 <- subset(df, attention=="1" | is.na(attention))
#ggplot(df1, aes(x=attention)) + geom_bar() + labs(title="Attention Check for Treatment") 

###########################################################
###########################################################
###########################################################
############# Variable Cleaning and Recoding ##############
###########################################################
###########################################################
##########################################################

#######################
##Relevant Variables##
#######################

#Traits

df$ideo_gene_percent <- df$trait_proportion_1_1
df$ideo_env_percent <- df$trait_proportion_1_2
df$ideo_cont_percent <- df$trait_proportion_1_3
df$g <- df$ideo_gene_percent/100 #Helps Ease Aid of Interpretation Later


df$height_gene_percent <- df$trait_proportion_2_1
df$height_env_percent <- df$trait_proportion_2_2
df$height_cont_percent <- df$trait_proportion_2_3

df$pid_gene_percent <- df$trait_proportion_3_1
df$pid_env_percent <- df$trait_proportion_3_2
df$pid_cont_percent <- df$trait_proportion_3_3

df$int_gene_percent <- df$trait_proportion_4_1
df$int_env_percent <- df$trait_proportion_4_2
df$int_cont_percent <- df$trait_proportion_4_3

df$crime_gene_percent <- df$trait_proportion_5_1
df$crime_env_percent <- df$trait_proportion_5_2
df$crime_cont_percent <- df$trait_proportion_5_3

#Affective Partisan Polarization

df$reptherm<- df$partyfeel_1
df$demtherm <-df$partyfeel_2
df$intherm <- df$partyfeel_3
df$party_gap <- df$reptherm-df$demtherm

#Affective Ideological Polarization

df$libtherm<- df$ideofeel_1
df$contherm <-df$ideofeel_2
df$modtherm <- df$ideofeel_3
df$ideo_gap <- df$contherm-df$libtherm

#Social Distance (Higher Values = More Happy)

df$demdist <- df$social_dist_1
df$repdist <- df$social_dist_2
df$condist <- df$social_dist_3
df$libdist <- df$social_dist_4

#Moral Distance (High Values = Greater Perception of Difference)

df$moral_dist <- df$moral_dist_1
df$honest_dist <- df$moral_dist_2
df$trust_dist <- df$moral_dist_3
df$sincere_dist <- df$moral_dist_4
df$policy_dist <- df$moral_dist_5

#Partisan Dehumanization (Republican - Animalistic; First 3 = Good Traits)

df$composite_ra <- (df$dehumize_rep_1 + 
                    df$dehumize_rep_2 + 
                    df$dehumize_rep_3 - 
                    df$dehumize_rep_4 - 
                    df$dehumize_rep_5 - 
                    df$dehumize_rep_6)

#Partisan Dehumanization (Republican - Mechanistic; First 3 = Good Traits)

df$composite_rm <- (df$dehumize_rep_7 + 
                    df$dehumize_rep_8 + 
                    df$dehumize_rep_9 - 
                    df$dehumize_rep_10 - 
                    df$dehumize_rep_11 - 
                    df$dehumize_rep_12)

#Partisan Dehumanization (Democrat - Animalistic)

df$composite_da <- (df$dehumanize_dem_1 + 
                    df$dehumanize_dem_2 + 
                    df$dehumanize_dem_3 - 
                    df$dehumanize_dem_4 - 
                    df$dehumanize_dem_5 - 
                    df$dehumanize_dem_6)

#Partisan Dehumanization (Democrat - Mechanistic)

df$composite_dm <- (df$dehumanize_dem_7 + 
                    df$dehumanize_dem_8 + 
                    df$dehumanize_dem_9 - 
                    df$dehumanize_dem_10 - 
                    df$dehumanize_dem_11 - 
                    df$dehumanize_dem_12)

#Open Mindedness and Peruasion

df$rep_open <-df$dehumize_rep_13
df$rep_persuade <- df$dehumize_rep_14

df$dem_open <-    df$dehumanize_dem_13
df$dem_persuade <- df$dehumanize_dem_14

#Compromise Score (0 = Stick to Positon; 1 = Make Compromises)

df$compromise <- df$comp1

#Compromise Scale

df$repget <- df$comp2_2
df$demget <- df$comp2_3
df$elitedif<- df$repget - df$demget

#Seeking Out Other Views (0 = Very Unimportant; 4 = Very Important)

describe(df$seek_out)
describe(df$ideo_diverse)

###########################
#######Demographics########
###########################

#Party Indentification (Higher Values = More Republican)

df$pid7 <- "."
df$pid7[df$dem1==1] <-1
df$pid7[df$dem1==0] <-2
df$pid7[df$ind1==2] <-3
df$pid7[df$ind1==3] <-4
df$pid7[df$ind1==1] <-5
df$pid7[df$rep1==0] <-6
df$pid7[df$rep1==1] <-7
df$pid7 <- as.numeric(df$pid7)
hist(df$pid7)

#Center PID Variable for Regression (zscore)

df$pids<- scale(df$pid7, scale = TRUE)

#Strength of Party Identification

df$strength <- 0
df$strength[df$pid7==3 | df$pid7==5] = 1
df$strength[df$pid7==2 | df$pid7==6] = 2
df$strength[df$pid7==1 | df$pid7==7] = 3

#Political Knowledge Scale

df$pk <- df$know1 + df$know2

#Race (Score of 0 = Non-White; Score of 1 = White)

df$white <- "."
df$white[df$race!="." & df$race !=5] <- 0
df$white[df$race==5] <-1
df$white <- factor(df$white, levels = c(0,1), labels = c("Non-White", "White"))
df$white <- as.numeric(as.factor(df$white))
df$white[df$white==1] = 0
df$white[df$white==2] = 1
describe(df$white)

#Gender

df$male <- "."
df$male [df$gender!="." | df$gender ==0 | df$gender==99] <- 0
df$male [df$gender!="." & df$gender == 1] <- 1
df$male <- factor(df$male, levels = c(0,1), labels = c("Non-Male", "Male"))
df$male <- as.numeric(as.factor(df$male))
describe(df$male)
df$male[df$male==1] = 0
df$male[df$male==2] = 1
describe(df$male)

#Education

df$education <- "."
df$education[df$educ==1]<-1
df$education[df$educ==2]<-2
df$education[df$educ==3]<-3
df$education[df$educ==4]<-4
df$education[df$educ==5]<-5
df$education[df$educ==6]<-6
df$education[df$educ==7]<-7
df$education[df$educ==8]<-8
df$education <- factor(df$education, levels = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c("No HS", "HS", "Some College", "Associates", "BA", "MA", "Prof", "Ph.D."))
df$education <- as.numeric(as.factor(df$education))

#Ideology (Higher Values = More Conservative)

df$ideology <- "."
df$ideology[df$ideo==1]<-1
df$ideology[df$ideo==2]<-2
df$ideology[df$ideo==3]<-3
df$ideology[df$ideo==4]<-4
df$ideology[df$ideo==5]<-5
df$ideology[df$ideo==6]<-6
df$ideology[df$ideo==7]<-7
df$ideology <- factor(df$ideology, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("Very Liberal", "Liberal", "Somewhat Liberal", "Moderate", "Somewhat Conservative", "Conservative", "Very Conservative"))
df$ideology <- as.numeric(factor(df$ideo, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("Very Liberal", "Liberal", "Somewhat Liberal", "Moderate", "Somewhat Conservative", "Conservative", "Very Conservative")))
df$ideology <- as.numeric(as.factor(df$ideology))

#Trust in Science

df$science <- "."
df$science [df$science_trust == 20] <- 0
df$science [df$science_trust == 21] <- 1
df$science [df$science_trust == 22] <- 2
df$science [df$science_trust == 23] <- 3
df$science [df$science_trust == 24] <- 4
df$science <- as.numeric(as.factor(df$science))
describe(df$science)

#Dweck Scale

df$dweck<- df$d_person_1 + df$d_person_2 + df$d_person_3

#Summarize Covariates

describe(df$male) #Respondent Sex (Male Dummy Variable)
describe(df$white) #Respodent Race (White DUmmy Variable)
describe(df$education) #Respondent Education Level
describe(df$relig) #Respondent Religiosity
describe(df$pid7) #Respondent Partisan Affiliation 
describe(df$ideology) #Respondent Ideology
describe(df$ses) #Respondent Subjective Class Perception
describe(df$income) #Respondent Income Level
describe(df$employ) #Respondent Employment Status
describe(df$pk) #Respondent Political Knowledge Index
describe(df$interest) #Respondent Political Interest
describe(df$science) #Respondent Political Interest
describe(df$dweck) #Respondent Political Interest

#Merge Covariates into Same Dataframe

covariates <- subset(df, select = c(age, male, white, education, relig, pid7, ideology, ses, income, employ, pk, interest, science, dweck))
covariate.df <- data.frame(covariates)

#Generate Descriptive Statistics Table (Check Working Directory for .doc output)

stargazer(covariate.df, type = "latex", title="Summary Statistics", digits=1, out = "table1.tex")

#Generate Additional Data Subsets

treated <- subset(df, treat=="1")
control <- subset(df, treat=="0")
dem <- subset(df, pid7<="3")
rep <- subset(df, pid7>="5")

###########################################################
###########################################################
###########################################################
############### Preliminary Data Analysis #################
###########################################################
###########################################################
###########################################################

#Assess Covariate Balance

balance <- glm(treat ~ age + male + white + education + relig + pid7 + ideology + ses + income + employ + pk + interest, data = df, family = "binomial")
summary(balance)
stargazer(balance, title="Covariate Balance Check", align=TRUE, no.space=TRUE, single.row=TRUE)

#Assess Correlation Between Party ID & Ideology Trait Attributions

cor.test(df$pid_gene_percent, df$ideo_gene_percent, method=c("pearson", "kendall", "spearman"))

#Get Frequencies of Individuals with Score of 0 in Treatment and Control Group

table(control$ideo_gene_percent)
table(treated$ideo_gene_percent)                   

#See If There Are Differenences Once We Drop Attention Check Failures

df1 <- subset(df, attention=="1" | is.na(attention))
treated1 <- subset(df1, treat=="1")
table(treated1$ideo_gene_percent)                   

#Does Treatment Shift Genetic Perceptions? (Regressions and Figures 1 and 2)

ideogene <- lm(ideo_gene_percent ~ treat, data=df)
summary(ideogene)

pidgene <- lm(pid_gene_percent ~ treat, data=df)
summary(pidgene)

plot1 <- ggplot(data=df, mapping=aes(x=treat_char, y=ideo_gene_percent, fill=treat_char)) +
  stat_summary(fun.data=mean_sdl, geom="bar", width=.40) +
  labs (x="Condition", y="% Biological Trait Attribution") +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=.10) + 
  scale_x_discrete(labels = c("Control Group","Treatment Group")) 
plot1
plot1 + scale_fill_npg() + theme(legend.position = "none")
ggsave("plot1.jpeg", units="in", width=5, height=4, dpi=300)

plot2 <- ggplot(data=df, mapping=aes(x=treat_char, y=pid_gene_percent, fill=treat_char)) +
  stat_summary(fun.data=mean_sdl, geom="bar", width=.40) +
  labs (x="Condition", y="% Biological Trait Attribution") +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=.10) + 
  scale_x_discrete(labels = c("Control Group","Treatment Group")) 
plot2
plot2 + scale_fill_npg() + theme(legend.position = "none")
ggsave("plot2.jpeg", units="in", width=5, height=4, dpi=300)

#See Which Other Factors Predict Endorsement of Genetic Trait Attributions

ideogene <- lm(ideo_gene_percent ~ science + dweck + age + male + white + education + relig + pid7 + ideology + ses + income + employ + pk + interest, data=df)
summary(ideogene)
stargazer(ideogene, title="What Predicts Greater Endorsement of Genetic Trait Attributions?", align=TRUE, no.space=TRUE)

#Make Sure Treatment Does Not Influence Generalized Dweck Scores

dweckreg <- lm(dweck ~  treat + science + age + male + white + education + relig + pid7 + ideology + ses + income + employ + pk + interest, data=df)
summary(dweckreg)

###############################################################
###############################################################
###############################################################
#Do Genetic Trait Attributions Predict Affective Polarization?#
###############################################################
###############################################################
###############################################################

#Warmth Toward Republicans

af1 <- lm(reptherm ~ g * pid7 + dweck + science + age + male + white + education + relig  + ses + income + employ + pk , data=df)
summary(af1)

plot3 <- interplot(m = af1, var1 = "g", var2 = "pid7", point=T) + # Add labels for X and Y axes
  ylab("Warmth Toward Republicans") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90)) +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")

plot3 + scale_x_discrete(limits=c("1","2","3", "4", "5", "6","7"), labels=c("Strong Democrat", "Not Strong Democrat", "Leaning Democrat", "Independent", "Leaning Republican","Not Strong Republican", "Strong Republican"))
ggsave("plot3.jpeg", units="in", width=5, height=4, dpi=300)

#Warmth Toward Democrats

af2 <- lm(demtherm ~ g * pid7 + dweck + science + age + male + white + education + relig  + ses + income + employ + pk , data=df)
summary(af2)
plot4 <- interplot(m = af2, var1 = "g", var2 = "pid7", point=T) + # Add labels for X and Y axes
  ylab("Warmth Toward Democrats") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90)) +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")

plot4 + scale_x_discrete(limits=c("1","2","3", "4", "5", "6","7"), labels=c("Strong Democrat", "Not Strong Democrat", "Leaning Democrat", "Independent", "Leaning Republican","Not Strong Republican", "Strong Republican"))
ggsave("plot4.jpeg", units="in", width=5, height=4, dpi=300)

#Comfort With Interparty Marriage of Republican

dist1 <- lm(repdist ~ g * pid7 + dweck + science + age + male + white + education + relig  + ses + income + employ + pk , data=df)
summary(dist1)

plot5 <- interplot(m = dist1, var1 = "g", var2 = "pid7", point=T) + # Add labels for X and Y axes
  ylab("Social Distance Toward Republicans") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90)) +
  geom_hline(yintercept = 0, linetype = "dashed")

plot5 + scale_x_discrete(limits=c("1","2","3", "4", "5", "6","7"), labels=c("Strong Democrat", "Not Strong Democrat", "Leaning Democrat", "Independent", "Leaning Republican","Not Strong Republican", "Strong Republican"))
ggsave("plot5.jpeg", units="in", width=5, height=4, dpi=300)

#Comfort With Interparty Marriage of Democrat

#Generate Binary Variable for Unhappiness

df$unhappydem <- 0
df$unhappydem[df$demdist==0] = 1
df$unhappyrep <- 0
df$unhappyrep[df$repdist==0] = 1


demlogit <- glm(unhappydem ~ g * pid7 + dweck + science + age + male + white + education + relig  + ses + income + employ + pk, data = df, family = "binomial")
summary(demlogit)

plot6 <- interplot(m = demlogit, var1 = "g", var2 = "pid7", point=T) + # Add labels for X and Y axes
  ylab("Democrat Social Distance") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90)) +
  geom_hline(yintercept = 0, linetype = "dashed")

plot6 + scale_x_discrete(limits=c("1","2","3", "4", "5", "6","7"), labels=c("Strong Democrat", "Not Strong Democrat", "Leaning Democrat", "Independent", "Leaning Republican","Not Strong Republican", "Strong Republican"))
ggsave("plot6.jpeg", units="in", width=5, height=4, dpi=300)


#Republican Marriage

replogit <- glm(unhappyrep ~ g * pid7 + dweck + science + age + male + white + education + relig  + ses + income + employ + pk, data = df, family = "binomial")
summary(replogit)

plot7 <- interplot(m = replogit, var1 = "g", var2 = "pid7", point=T) + # Add labels for X and Y axes
  ylab("Republican Social Distance") +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90)) +
  geom_hline(yintercept = 0, linetype = "dashed")

plot7 + scale_x_discrete(limits=c("1","2","3", "4", "5", "6","7"), labels=c("Strong Democrat", "Not Strong Democrat", "Leaning Democrat", "Independent", "Leaning Republican","Not Strong Republican", "Strong Republican"))
ggsave("plot7.jpeg", units="in", width=5, height=4, dpi=300)

###########################################################
###########################################################
###########################################################
#Does Treatment Influence Affective Partisan Polarization?#
###########################################################
###########################################################
###########################################################

af1 <- lm(reptherm ~ treat * pid7, data=df)
af2 <- lm(demtherm ~ treat * pid7, data=df)
af3 <- lm(party_gap ~ treat * pid7, data=df)
stargazer(af1, af2, af3, title="Does Treatment Influence Affective Polarization?", align=TRUE, no.space=TRUE)

###########################################################
###########################################################
###########################################################
###############  Causal Mediation Analysis  ###############
###########################################################
###########################################################
###########################################################

#Mediation Among All Democrats 

dem$g<-(dem$ideo_gene_percent)/100
dem <- subset(dem, dem$pid7<="3")
m1 <- lm(g ~ treat, data = dem)
o1 <- lm(reptherm ~ treat + g, data = dem)
mout1 <- mediate(m1, o1, treat = "treat", mediator = "g", robustSE = TRUE, sims = 1000, conf.level = 0.90)
summary(mout1)
plot(mout1, main = "Wamrth Toward Republicans (All Democrats)")

#Mediation Among Strong Democrats 

dem <- subset(dem, dem$pid7=="1")
m2 <- lm(g ~ treat, data = dem)
o2 <- lm(reptherm ~ treat + g, data = dem)
mout2 <- mediate(m2, o2, treat = "treat", mediator = "g", robustSE = TRUE, sims = 1000, conf.level = 0.90)
summary(mout2)
plot(mout2, main = "Wamrth Toward Republicans (Strong Dems)")

#Plot Mediation Results (Democrats)

par(mfrow=c(1,2))
plot(mout1, main = "Republican Warmth (All Dems)", xlim=c(-10,20))
plot(mout2, main = "Republican Wamrth (Strong Dems)", xlim=c(-10,20))

#Mediation Among All Republicans 

rep <- subset(df, df$pid7>="5")
m3 <- lm(ideo_gene_percent ~ treat, data = rep)
o3 <- lm(demtherm ~ treat + ideo_gene_percent, data = rep)
mout3 <- mediate(m3, o3, treat = "treat", mediator = "ideo_gene_percent", robustSE = TRUE, sims = 1000, conf.level = 0.90)
summary(mout3)
plot(mout3, main = "Warmth Toward Democrats (All Republicans)")

rep <- subset(df, df$pid7=="7")
m4 <- lm(ideo_gene_percent ~ treat, data = rep)
o4 <- lm(demtherm ~ treat + ideo_gene_percent, data = rep)
mout4 <- mediate(m4, o4, treat = "treat", mediator = "ideo_gene_percent", robustSE = TRUE, sims = 1000, conf.level = 0.90)
summary(mout4)
plot(mout4, main = "Wamrth Toward Democrats (Strong Republicans)")

#Plot Mediation Results (Republicans)

par(mfrow=c(1,2))
plot(mout3, main = "Democrat Warmth (All Reps)", xlim=c(-10,20))
plot(mout4, main = "Democrat Wamrth (Strong Reps)", xlim=c(-10,20))

rep <- subset(df, df$pid7>="5")
rep$af <- rep$reptherm - rep$demtherm

m5 <- lm(ideo_gene_percent ~ treat, data = rep)
o5 <- lm(af ~ treat + ideo_gene_percent, data = rep)
mout5 <- mediate(m5, o5, treat = "treat", mediator = "ideo_gene_percent", robustSE = TRUE, sims = 1000, conf.level = 0.90)
summary(mout5)
plot(mout5, main = "Affective Polarization Among Republicans")

dem <- subset(df, df$pid7<="3")
dem$af <- dem$demtherm - dem$reptherm
m6 <- lm(ideo_gene_percent ~ treat, data = rep)
o6 <- lm(af ~ treat + ideo_gene_percent, data = rep)
mout6 <- mediate(m6, o6, treat = "treat", mediator = "ideo_gene_percent", robustSE = TRUE, sims = 1000, conf.level = 0.90)
summary(mout6)
plot(mout6, main = "Affective Polarization Among Democrats")

med.fit <- lm(ideo_gene_percent ~ treat, data = dem)
out.fit <- lm(repdist ~ treat + ideo_gene_percent, data = dem)
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "ideo_gene_percent", robustSE = TRUE, sims = 1000, conf.level = 0.90)
summary(med.out)
plot(med.out, main = "Causal Mediation on Social Distance Toward Republicans")


med.fit <- lm(ideo_gene_percent ~ treat, data = rep)
out.fit <- lm(demdist ~ treat + ideo_gene_percent, data = rep)
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "ideo_gene_percent", robustSE = TRUE, sims = 1000, conf.level = 0.90)
summary(med.out)
plot(med.out, main = "Causal Mediation on Social Distance Toward Democrats")


med.fit <- lm(ideo_gene_percent ~ treat, data = rep)
out.fit <- lm(elitedif ~ treat + ideo_gene_percent, data = rep)
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "ideo_gene_percent", robustSE = TRUE, sims = 1000, conf.level = 0.90)
summary(med.out)
plot(med.out, main = "Causal Mediation on Attitudes Toward Compromise")

#Effect of Treatment on Compromise Preferences

comp1 <- glm(compromise ~ treat , data = df, family = "binomial")
summary(compreg)

comp2 <- lm(elitedif ~ treat*pid7, data=df)
summary(comp2)

vp1 <- lm(seek_out ~ treat, data=df)
summary(vp1)

stargazer(comp1, comp2, vp1, title="", align=TRUE, no.space=TRUE, single.row=FALSE)



#Multiple Mediation




oh <-  multimed("reptherm", "rep_open", "ideo_gene_percent", "treat", experiment = NULL,
                data = dem, design = "single", sims = 1000, conf.level = 0.90)
summary(oh)
plot(oh, tg = "av")

oh <-  multimed("demtherm", "dem_open", "ideo_gene_percent", "treat", experiment = NULL,
                data = rep, design = "single", sims = 1000, conf.level = 0.90)
summary(oh)
plot(oh, tg = "av")

oh <-  multimed("demdist", "dem_open", "ideo_gene_percent", "treat", experiment = NULL,
                data = rep, design = "single", sims = 1000, conf.level = 0.90)
summary(oh)
plot(oh, tg = "av")

#Does Treatment Influence Perceptions of Social Distance?

sd1 <- lm(demdist ~ treat * pid7, data=df1)
summary(sd1)

sd2 <- lm(repdist ~ treat * pid7, data=df1)
summary(sd2)

#Social Distance Plots

interplot(m = sd1, var1 = "treat", var2 = "pid7", point = T) + # Add labels for X and Y axes
  xlab("Party Identification") +
  ylab("Happiness with Marrying a Democrat") +
  # Change the background
  theme_bw() +
  # Add the title
  ggtitle("ATE of Exposure on Social Distance Toward Democrats") +
  theme(plot.title = element_text(face="bold")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

interplot(m = sd2, var1 = "treat", var2 = "pid7", point=T) + # Add labels for X and Y axes
  xlab("Party Identification") +
  ylab("Happiness with Marrying a Republican") +
  # Change the background
  theme_bw() +
  # Add the title
  ggtitle("ATE of Exposure on Social Distance Toward Republicans") +
  theme(plot.title = element_text(face="bold")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed")

#Does Treatment Influence Moral Distance Perceptions?

md1 <- lm(df$moral_dist ~ g*pid7, data=df)
summary(md1)
md2 <- lm(df$honest_dist ~ g*pid7, data=df)
summary(md2)
md3 <- lm(df$trust_dist ~ g*pid7, data=df)
summary(md3)
md4 <- lm(df$sincere_dist~ g*pid7, data=df)
summary(md4)
md5 <- lm(df$policy_dist ~ g*pid7, data=df)
summary(md5)

md1 <- lm(df$moral_dist ~ treat*pid7, data=df)
summary(md1)
md2 <- lm(df$honest_dist ~ treat*pid7, data=df)
summary(md2)
md3 <- lm(df$trust_dist ~ treat*pid7, data=df)
summary(md3)
md4 <- lm(df$sincere_dist~ treat*pid7, data=df)
summary(md4)
md5 <- lm(df$policy_dist ~ treat*pid7, data=df)
summary(md5)

#Does Treatment Influence Dehumanization? (Higher Values = Less Dehumanization)

dehumanize_rep_animal <- lm(composite_ra ~ treat*pid7, data=df)
summary(dehumanize_rep_animal)

dehumanize_rep_mechanistic <- lm(composite_rm ~ treat*pid7, data=df)
summary(dehumanize_rep_mechanistic)

dehumanize_dem_animal <- lm(composite_da ~ treat*pid7, data=df)
summary(dehumanize_dem_animal)

dehumanize_dem_mechanistic <- lm(composite_dm ~ treat*pid7, data=df)
summary(dehumanize_dem_mechanistic)

#Dehumanization Plots

interplot(m = dehumanize_dem_animal, var1 = "treat", var2 = "pid7", point = T) + # Add labels for X and Y axes
  xlab("Party Identification") +
  ylab("Democrat Dehumanization") +
  theme_bw() +
  ggtitle("Estimated Effect of Exposure on Subtle Dehumanization") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")

#Does Treatment Influence Perceptions of Open-Mindedness?

ropen <- lm(rep_open ~ treat * pid7, data=df)
summary(ropen)

interplot(m = ropen, var1 = "treat", var2 = "pid7", point = T) + # Add labels for X and Y axes
  xlab("Party Identification") +
  ylab("Republican Openness") +
  theme_bw() +
  ggtitle("ATE on Beliefs That Rs Are Open-Minded") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")

dopen <- lm(dem_open ~ treat * pid7, data=df)
summary(dopen)

interplot(m = dopen, var1 = "treat", var2 = "pid7", point = T) + # Add labels for X and Y axes
  xlab("Party Identification") +
  ylab("Democrat Openness") +
  theme_bw() +
  ggtitle("ATE on Beliefs That Ds Are Open-Minded") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")

#Does Treatment Influence Perceptions of Persuadability?

rper <- lm(rep_persuade ~ treat*pid7, data=df)
summary(rper)

interplot(m = rper, var1 = "treat", var2 = "pid7", point = T) + # Add labels for X and Y axes
  xlab("Party Identification") +
  ylab("Republican Persuadability") +
  theme_bw() +
  ggtitle("ATE on Beliefs That Rs Are Persuadable") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")

dper <- lm(dem_persuade ~ treat * pid7, data=df)
summary(dper)

interplot(m = dper, var1 = "treat", var2 = "pid7", point = T) + # Add labels for X and Y axes
  xlab("Party Identification") +
  ylab("Democrat Persuadability") +
  theme_bw() +
  ggtitle("ATE on Beliefs That Ds Are Persuadable") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")

#Does Treatment Shift Preferences for Compromise?

c1 <- lm(df$compromise ~ treat * pid7, data=df)
summary(c1)

c2 <- lm(df$elitedif ~ treat*pid7, data=df)
summary(c2)

interplot(m = c2, var1 = "treat", var2 = "pid7", point = T) + # Add labels for X and Y axes
  xlab("Party Identification") +
  ylab("Difference") +
  theme_tufte() +
  ggtitle("ATE on Elite Compromise") +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 0, linetype = "dashed")

#Does Treatment Influence Preferences Over Ideological Diversity?

vp1 <- lm(df$seek_out ~ ideo_gene_percent, data=df)
summary(vp1)

vp2 <- lm(df$ideo_diverse ~ ideo_gene_percent, data=df)
summary(vp2)

#Split Sample Regressions

dem1 <- lm(reptherm ~ treat, data=subset(df, pid7=="1"))
summary(dem1)
dem2 <- lm(reptherm ~ treat, data=subset(df, pid7=="2"))
summary(dem2)
dem3 <- lm(reptherm ~ treat, data=subset(df, pid7=="3"))
summary(dem3)
rep1 <- lm(demtherm ~ treat, data=subset(df, pid7=="5"))
summary(rep1)
rep2 <- lm(demtherm ~ treat, data=subset(df, pid7=="6"))
summary(rep2)
rep3 <- lm(demtherm ~ treat, data=subset(df, pid7=="7"))
summary(rep3)

dem1 <- lm(party_gap ~ treat, data=subset(df, pid7=="1"))
summary(dem1)
dem2 <- lm(party_gap ~ treat, data=subset(df, pid7=="2"))
summary(dem2)
dem3 <- lm(party_gap ~ treat, data=subset(df, pid7=="3"))
summary(dem3)
rep1 <- lm(party_gap ~ treat, data=subset(df, pid7=="5"))
summary(rep1)
rep2 <- lm(party_gap ~ treat, data=subset(df, pid7=="6"))
summary(rep2)
rep3 <- lm(party_gap ~ treat, data=subset(df, pid7=="7"))
summary(rep3)

dem1 <- lm(party_gap ~ ideo_gene_percent, data=subset(df, pid7=="1"))
summary(dem1)
dem2 <- lm(party_gap ~ treat, data=subset(df, pid7=="2"))
summary(dem2)
dem3 <- lm(party_gap ~ treat, data=subset(df, pid7=="3"))
summary(dem3)
rep1 <- lm(party_gap ~ treat, data=subset(df, pid7=="5"))
summary(rep1)
rep2 <- lm(party_gap ~ treat, data=subset(df, pid7=="6"))
summary(rep2)
rep3 <- lm(party_gap ~ treat, data=subset(df, pid7=="7"))
summary(rep3)

affective <- lm(party_gap ~ ideo_gene_percent*pid7, data = df)
summary(affective)

dem1 <- lm(repdist ~ treat, data=subset(df, pid7=="1"))
summary(dem1)
dem2 <- lm(repdist ~ treat, data=subset(df, pid7=="2"))
summary(dem2)
dem3 <- lm(repdist ~ treat, data=subset(df, pid7=="3"))
summary(dem3)
rep1 <- lm(demdist ~ treat, data=subset(df, pid7=="5"))
summary(rep1)
rep2 <- lm(demdist ~ treat, data=subset(df, pid7=="6"))
summary(rep2)
rep3 <- lm(demdist ~ treat, data=subset(df, pid7=="7"))
summary(rep3)

dem1 <- lm(repdist ~ ideo_gene_percent, data=subset(df, pid7=="1"))
summary(dem1)
dem2 <- lm(repdist ~ ideo_gene_percent, data=subset(df, pid7=="2"))
summary(dem2)
dem3 <- lm(repdist ~ ideo_gene_percent, data=subset(df, pid7=="3"))
summary(dem3)
rep1 <- lm(demdist ~ ideo_gene_percent, data=subset(df, pid7=="5"))
summary(rep1)
rep2 <- lm(demdist ~ ideo_gene_percent, data=subset(df, pid7=="6"))
summary(rep2)
rep3 <- lm(demdist ~ ideo_gene_percent, data=subset(df, pid7=="7"))
summary(rep3)



dem1 <- lm(moral_dist ~ treat, data=subset(df, pid7=="1"))
summary(dem1)
dem2 <- lm(moral_dist ~ treat, data=subset(df, pid7=="2"))
summary(dem2)
dem3 <- lm(moral_dist ~ treat, data=subset(df, pid7=="3"))
summary(dem3)
rep1 <- lm(moral_dist ~ treat, data=subset(df, pid7=="5"))
summary(rep1)
rep2 <- lm(moral_dist ~ treat, data=subset(df, pid7=="6"))
summary(rep2)
rep3 <- lm(moral_dist ~ treat, data=subset(df, pid7=="7"))
summary(rep3)


compreg <- glm(compromise ~ treat , data = df, family = "binomial")
summary(compreg)
stargazer(compreg, title="Compromise Preferences", align=TRUE, no.space=TRUE, single.row=TRUE)



rep3 <- lm(compromise ~ ideo_gene_percent, data=df)
summary (rep3)

dem1 <- lm(elitedif ~ treat*pid7, data=df)
summary(dem1)

dem1 <- lm(seek_out ~ treat, data=df)
summary(dem1)




dem1 <- lm(elitedif ~ treat, data=subset(df, pid7=="1"))
summary(dem1)
dem2 <- lm(elitedif ~ treat, data=subset(df, pid7=="2"))
summary(dem2)
dem3 <- lm(elitedif ~ treat, data=subset(df, pid7=="3"))
summary(dem3)
rep1 <- lm(elitedif ~ treat, data=subset(df, pid7=="5"))
summary(rep1)
rep2 <- lm(elitedif ~ treat, data=subset(df, pid7=="6"))
summary(rep2)
rep3 <- lm(elitedif ~ treat, data=subset(df, pid7=="7"))
summary(rep3)

dem1 <- lm(seek_out ~ treat, data=subset(df, pid7=="1"))
summary(dem1)
dem2 <- lm(seek_out ~ treat, data=subset(df, pid7=="2"))
summary(dem2)
dem3 <- lm(seek_out ~ treat, data=subset(df, pid7=="3"))
summary(dem3)
rep1 <- lm(seek_out ~ treat, data=subset(df, pid7=="5"))
summary(rep1)
rep2 <- lm(seek_out ~ treat, data=subset(df, pid7=="6"))
summary(rep2)
rep3 <- lm(seek_out ~ treat, data=subset(df, pid7=="7"))
summary(rep3)

dem1 <- lm(ideo_diverse ~ treat, data=subset(df, pid7=="1"))
summary(dem1)
dem2 <- lm(ideo_diverse ~ treat, data=subset(df, pid7=="2"))
summary(dem2)
dem3 <- lm(ideo_diverse ~ treat, data=subset(df, pid7=="3"))
summary(dem3)
rep1 <- lm(ideo_diverse ~ treat, data=subset(df, pid7=="5"))
summary(rep1)
rep2 <- lm(ideo_diverse ~ treat, data=subset(df, pid7=="6"))
summary(rep2)
rep3 <- lm(ideo_diverse ~ treat, data=subset(df, pid7=="7"))
summary(rep3)

