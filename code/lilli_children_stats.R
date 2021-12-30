## Information about the data:
## The data set is cleaned (excluded data points outside AoI's;
##                          excluded trials with more than 50% data loss)
## Restriction to relevant time window -- between 3853ms (200ms after offset of the relative pronoun)
##                                        and 7000ms (end of the silence that followed the sentence)

#-------------------------

rm(list=ls())

## loading the data
load("lilli_children.RData")
#load("lilli_adults.RData")

library(dplyr)

## excluding a participant who doesn't have scores on the standardized tests
dat.c <- dat.c %>%
  filter(id != 'C_23') %>%
  droplevels()

tmp <- dat.c

## bin size should be at least double the sample size (60Hz: 16.6*2 = 33.2)
## 1.) from sentence onset until end of silence
tmp <- tmp %>%
  filter(ms <= 7000) %>%
  droplevels()

## 2.) from 200ms after offset of "che" until end of silence
## onset of "che" is at 3560ms
## mean duration of "che" is 130ms
## taking 200ms as average time of programming and executing an eye movement

# tmp <- dat.c %>%
#   filter(ms > 3560+130+200 & ms <= 7000) %>%
#   droplevels()

## 3.) from 200ms after offset of "che" until end of silence minus the last 1000ms.
## Based on the plot, the last second of the silence does not contain different effects, which occur earlier
# tmp <- dat.c %>%
#   filter(ms < 3560+130+200 & ms <= 4850+650)
  
# binsize <- 50
# tmp$bin <- floor(tmp$ms/binsize)*binsize

# tmp <- tmp %>%
#   filter(condition != 'filler') %>%
#   droplevels()

## subject relatives vs object relatives
# tmp$sentence_type <- factor( 
#                        ifelse(
#                          tmp$condition=="SR+2DP" | 
#                          tmp$condition=="SR+3pro" | 
#                          tmp$condition=="SR+1pro", "SR", "OR") )

## pronoun type
# tmp$ref <- factor( 
#              ifelse(
#                tmp$condition=="SR+1pro" | 
#                tmp$condition=="OR+1pro", "1pro",
#                ifelse(
#                  tmp$condition=="SR+3pro" | 
#                  tmp$condition=="OR+3pro", "3pro", "DP")) )

#---------------------------------

# cols_to_exclude <- c('group','aoi.target','aoi.distractor',
#                     'aoi.middle', 'accuracy', 'tcgb_total')
# 
# tmp2 <- tmp %>%
#   select(-cols_to_exclude)
# 
# save(tmp2, file='children.RData')

tmp <- tmp %>%
  select(-tcgb_total)

tcgb_cols <- colnames(tmp[,grepl('tcgb',colnames(tmp))])
memory_cols <- colnames(tmp[,grepl('span',colnames(tmp))])

tmp$tcgb_avg <- (tmp$tcgb_locative + tmp$tcgb_flessionali + 
                 tmp$tcgb_attive_affermative + tmp$tcgb_attive_negative + 
                 tmp$tcgb_passive_affermative + tmp$tcgb_passive_negative + 
                 tmp$tcgb_relative + tmp$tcgb_dative) / 8

tmp$memory_avg <- (tmp$block_span + tmp$digit_span) / 2

cols_to_exclude <- c('group','Time','Timing','Latency',
                     'Pupil.Confidence','L.Event.Info','R.Event.Info',
                     'x','y','pos','onscreen','aoi.le','aoi.ri',
                     'aoi.mi','timing','mv','response',
                     tcgb_cols, memory_cols)

tmp2 <- tmp %>%
  select(-cols_to_exclude)

binsize <- 50
tmp2$bin <- floor(tmp2$ms/binsize)*binsize

agg <- tmp2 %>%
  group_by(id, bin,condition) %>%
  summarize(t = mean(aoi.target),
            d = mean(aoi.distractor),
            m = mean(aoi.middle))

library(tidyverse)
agg2 <- gather(agg, aoi, M, t:m, factor_key=TRUE)

agg3 <- agg2 %>%
  group_by(bin, condition, aoi) %>%
  summarize(M = mean(M))

levels(agg3$aoi) <- c('Target','Distractor','Middle')

library(ggplot2)
ggplot(subset(agg3,condition=='SR+2DP'), 
       aes(x=bin, y=M, group=aoi, color=aoi)) + 
  geom_line(size=1.5) + 
  scale_colour_manual(values=c("#009E73","#D55E00","#999999"), name="Area of Interest") + 
  xlab('Time in ms') + 
  ylab('Proportion of looks') + 
  theme_bw() + 
  geom_vline(xintercept = 3653, size=1, color='black',linetype='dashed') + 
  theme(
    axis.title = element_text(color='black',size=24),
    axis.text = element_text(color='black',size=24),
    legend.title = element_text(color='black',size=20),
    legend.text = element_text(color='black',size=20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
  
agg_v2 <- tmp2 %>%
  group_by(bin,condition) %>%
  summarize(M = mean(aoi.target),
            SD = sd(aoi.target),
            N = n(),
            CI = SD/sqrt(N)*qt(.975, N-1) )

agg_v2 <- agg_v2[!is.na(agg_v2$CI),]

agg_v2$sentence_type <- agg_v2$condition
levels(agg_v2$sentence_type) <- c('1','2','3','4','5','6','7')

colors <-c("#999999", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#CC79A7")

ggplot(agg_v2, 
       aes(x=bin, y=M, group=sentence_type, color=sentence_type)) + 
  geom_line(size=1.5) + 
  xlab('Time in ms') + 
  ylab('Proportion of looks to Target area') + 
  scale_colour_manual(values=colors, name="Sentence Type") + 
  theme_bw() + 
  geom_vline(xintercept = 3653, size=1, color='black',linetype='dashed') + 
  theme(
    axis.title = element_text(color='black',size=24),
    axis.text = element_text(color='black',size=24),
    legend.title = element_text(color='black',size=20),
    legend.text = element_text(color='black',size=20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )



#---------------------------------

## Aggregating
agg <- tmp %>%
  group_by(bin, sentence_type, ref) %>%
  summarize(M = mean(aoi.target),
            SD = sd(aoi.target),
            N = n(),
            CI = SD/sqrt(N)*qt(.975, N-1)) %>%
  filter(N != 1) %>%
  droplevels()

agg$sentence_type <- factor(agg$sentence_type, levels=c("SR","OR"))
levels(agg$sentence_type) <- c("subject relatives", "object relatives")

levels(agg$ref) <- c("1st-person pronoun","3rd-person pronoun","lexical noun phrase")

agg$bin_cent <- agg$bin - min(agg$bin)
agg$bin.sec <- agg$bin_cent/1000

library(ggplot2)
library(grid)

## plotting
ggplot(data=agg,
       geom="smooth", method=lm, formula=y~poly(x,3),
       aes(x=bin_cent, y=M, color=ref, linetype=ref)) + 
  
  ylab("Proportion of looks to target") + 
  xlab("Time (sec.)") + theme_bw() + facet_grid(.~sentence_type) + 
  ylim(0,1) + 
  #scale_colour_manual(values=c("#707070","#000000","#707070","#000000"), name="RC:Pronoun") + 
  #scale_colour_manual(values=c("#009E73","#D55E00","#999999"), name="Embedded\nconstituent") + 
  scale_colour_manual(values=c("#009E73","#D55E00","#999999"), name="Embedded\nconstituent") + 
  #scale_linetype_manual(values=c(1,2), name="Pronoun") + 
  scale_linetype_manual(values=c("F1","dashed","solid"), name="Embedded\nconstituent") + 
  
  geom_smooth(method=lm, size=2, formula=y~poly(x,3)) + 
  geom_point(size=1)

################################################################################################
                                        ### STATISTICS ###
################################################################################################

library(plyr)

stat <- ddply(subset(dat.c,pres_ord2_div=="early"), 
              .(id, bin ,gap, ref), summarize,
              t=sum(aoi.target), d=sum(aoi.distractor), m=sum(aoi.middle))

stat <- droplevels(stat)

# sum of looks to patient
stat$y <- stat$t

# sum of looks to all AoI's
stat$N <- stat$t + stat$d + stat$m

# empirical logit
stat$elog <- log( (stat$y + .5) / (stat$N - stat$y + .5) )

# weights
stat$wts <- 1 / (stat$y+.5) + 1 / (stat$N - stat$y + .5)

# time variable in seconds
stat$sec <- stat$bin/1000

# centering time variable - around 4400ms for target looks
stat$sec.cent <- stat$sec - 4.4 # children

# scaling standardized measures
#stat$digit_span_cent <- scale(stat$digit_span, scale=F)
#stat$age.cent <- scale(stat$age, scale=T)

library(MASS)

# contrast for Gap
#levels(stat$gap)
#stat$gap <- factor(stat$gap, levels=c("SR", "OR"))
( contrasts(stat$gap) <- contr.sdif(2) ) #OR-SR

#stat$rc_type <- ifelse(stat$gap=="OR", 0.5, -0.5)

# contrast for Reference Type
stat$ref <- factor(stat$ref, levels=c("1pro","DP","3pro"))
( contrasts(stat$ref) <- contr.sdif(3) ) # 2-1 = DP-1pro
                                         # 3-2 = 3pro-DP

#levels(stat$pres_ord2_div)
( contrasts(stat$pres_ord2_div) <- contr.sdif(2) ) # 2-1 = late-early

#-------------------------------------------------
## setting contrast matrix for factor Condition ##

#levels(stat$condition)
#cmat <- matrix(c(-1,+1,0,0,0,0, # OR+2DP vs. SR+2DP
#                 -1,0,+1,0,0,0, # SR+3pro vs. SR+2DP
#                 -1,0,0,0,+1,0, # SR+1pro vs. SR+2DP
#                 0,-1,0,+1,0,0, # OR+3pro vs. OR+2DP
#                 0,-1,0,0,0,+1), # OR+1pro vs. OR+2DP
#               nrow=6,ncol=5,byrow=F)

#cmat <- fractions(t(ginv(cmat)))
#colnames(cmat) <- c("OR+2DP-SR+2DP","SR+3pro-SR+2DP","SR+1pro-SR+2DP","OR+3pro-OR+2DP","OR+1pro-OR+2DP")
#rownames(cmat) <- levels(stat$condition)
#(contrasts(stat$condition) <- cmat)
#-------------------------------------------------

## LMM ##
library(lme4)

m1c <- lmer(elog ~ poly(sec.cent,3)*gap*ref +
              
           (1+gap+ref|id), 
              
           data=stat, REML=FALSE, weights=1/wts, lmerControl(optimizer="bobyqa"))

anova(m1c,m1c) # model with linear/cubic/quadratic terms is preferable

#-------------------------------------------------------------------

print(summary(m1c), corr=FALSE)

#getwd()
setwd("C:/Users/yairh/Desktop/Publications/Lilli_Lucio - italian_relatives/version2 - 20160601/new_manuscript - July 2016/paper_parts")
write.csv(summary(m1c)$coefficients, file="children_LMM_by-subject.csv")
write.csv(summary(m1c)$coefficients, file="children_LMM_by-item.csv")


# plotting residuals
library(car)
qqPlot(resid(m1c))

plot(resid(m1) ~ fitted(m1))
abline(0,0, col="red")

#--------------------------------------------------------------
## Getting p-values for t-values of the model ##

# from Barr's tutorial on analyzing ET visual-world data (most recent update: July 2012)
# Walkthrough of an "empirical logit" analysis in R: http://talklab.psy.gla.ac.uk/tvw/elogit-wt.html

# getting approximation of p-values for the model
ts <- fixef(m1a) / sqrt(diag(vcov(m1a)))
print(round(ts,2))
ps <- 2*(1-pnorm(abs(ts)))
print(round(ps,10))

# adjusting p-values for multiple testing
adj <- p.adjust(ps, method="bonferroni", n=length(ps))
print(round(adj,10))

#----------------------------
# another way to get p-values
library(lmerTest)
summary(m1a)

# extracting p-values from model - !!!NOT SURE THIS WORKS FINE!!!
pv <- summary(m1a)$coef[, "Pr(>|t|)"]

# adjusting p-values for multiple testing
adj2 <- p.adjust(pv, method="bonferroni", n=length(pv))
print(round(adj2,5))

#--------------------------------------------------------------

# names of fixed factors
matrix(names(fixef(m1c)))

# removing individual differences
source("C:/Users/yairh/Desktop/Experiments/remef.v0.6.10.R")
stat$ia <- remef(m1c, keep=TRUE, fix=1:16, ran=NULL)

# stat$condition <- factor( paste(stat$gap, ":", stat$ref, sep="") )
# stat$condition <- factor(stat$condition, levels=c("SR:1pro","OR:1pro","SR:3pro","OR:3pro"))

stat$time <- stat$sec.cent + .55

stat$gap <- factor(stat$gap, levels=c("SR","OR"))
levels(stat$gap) <- c("Subject relatives","Object relatives")

levels(stat$ref) <- c("1st-person","3rd-person")

# plotting again after remef
library(ggplot2)
library(grid)

#-------------------------------------------------------------------------------
#plot2 <- ggplot(data=stat,
#                aes(x=sec.cent, y=ia, linetype=ref, shape=ref)) + facet_grid(.~gap) + 
#  
#         geom_smooth(method=lm, size=1, formula=y~poly(x,3)) +
#         
#         ylab("Adjusted proportion of looks to target") + 
#         xlab("Time in ms") + theme_bw() + #facet_grid(group~.) + 
#         ylim(0,1) + 
#  
#         #scale_colour_manual(values=c("#000000","#707070","#000000","#707070"), name="Condition") + 
#         scale_linetype_manual(values=c("solid","dashed","dotted"), name="Embedded\nreferring\nexpression") + 
#         scale_shape_manual(values=c(3,4,5), name="Embedded\nreferring\nexpression")

#---------------------------------------
plot2 <- ggplot(data=stat, geom="smooth", method=lm, formula=y~poly(x,3),
                aes(x=time, y=ia, linetype=ref, color=ref)) + 
  
  ylab("Adjusted proportion of target looks") + 
  xlab("Time (sec.)") + theme_bw() + facet_grid(.~gap) + 
  #scale_colour_manual(values=c("#707070","#000000","#707070","#000000"), name="RC:Pronoun") + 
  scale_colour_manual(values=c("#0072B2","#D55E00"), name="Pronoun") + 
  #scale_linetype_manual(values=c(1,1,2,2), name="RC:Pronoun") #+
  scale_linetype_manual(values=c(1,2), name="Pronoun")

plot2 <- plot2 + geom_smooth(method=lm, size=2.5, formula=y~poly(x,3))

#----------------------------------------------------------------

## additional lines in plot ##
# sentence onset
#plot1 <- plot1 + geom_vline(aes(xintercept=2000), linetype=2, size=1, col="black")

# offset of "che" - onset of embedded DP
#plot1 <- plot1 + geom_vline(aes(xintercept=3523+130), size=1, linetype=2, col="black")

# embedded verb onset
#plot1 <- plot1 + geom_vline(aes(xintercept=3523+130+250), size=1, linetype=2, col="black")

# silence onset
plot2 <- plot2 + geom_vline(aes(xintercept=(4850/1000-4.4)+.55), size=1, linetype=2, col="black") # sentence offset

## Aesthetics ##
# Bigger axis titles
plot2 <- plot2 + theme(axis.title.x=element_text(size=26, angle=0))
plot2 <- plot2 + theme(axis.title.y=element_text(size=26, angle=90))
# Bigger text on axes
plot2 <- plot2 + theme(axis.text.x=element_text(size=20, colour="black"))
plot2 <- plot2 + theme(axis.text.y=element_text(size=20, colour="black"))
# Bigger legend
plot2 <- plot2 + theme(legend.title=element_text(size=24))
plot2 <- plot2 + theme(legend.text=element_text(size=22))
# Legend position
plot2 <- plot2 + theme(legend.justification=c(0,1), legend.position=c(0,1))
# Box around legend
plot2 <- plot2 + theme(legend.background=element_rect(fill="transparent"))
plot2 <- plot2 + theme(legend.key.width = unit(2.5, "cm"))
# Bigger text in facets
plot2 <- plot2 + theme(strip.text.x = element_text(size=24, angle=0))
plot2 <- plot2 + theme(strip.text.y = element_text(size=24, angle=315))

# removing background grid
plot2 <- plot2 + theme(panel.grid.major=element_blank()) + 
  theme(panel.grid.minor=element_blank()) + 
  theme(panel.background=element_blank())

plot2

# export: 1000x650

#-------------------------------------------------------------------------------
plot2 <- plot2 + scale_colour_manual(values=c("#000000", "#707070", "#BDBDBD"), name="Referring\nExpression")
plot2 <- plot2 + scale_fill_manual(values=c("#000000", "#707070", "#BDBDBD"), name="Referring\nExpression")

plot2 <- plot2 + scale_colour_manual(values=c("#0072B2","#009E73","#D55E00"), name="Referring\nExpression")
plot2 <- plot2 + scale_fill_manual(values=c("#0072B2","#009E73","#D55E00"), name="Referring\nExpression")

# Colors for AMLaP 2015 poster (like Tenerife 2013) #
plot2 <- plot2 + scale_colour_manual(values=c("#800000", "#2121D9"), name="Pronoun")
plot2 <- plot2 + scale_fill_manual(values=c("#800000", "#2121D9"), name="Pronoun")

plot2 <- plot2 + scale_colour_manual(values=c("#000000", "#707070"), name="Pronoun")
plot2 <- plot2 + scale_fill_manual(values=c("#000000", "#707070"), name="Pronoun")
#-------------------------------------------------------------------------------
