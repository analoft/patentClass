#making data set for text analysis
#setwd("/Users/kartikeya kirar/Desktop/ajeet/ajeet_project/  ")
source("script/support_function.R")

dat<-read_excel("data/181007 ip data confidential do not share.xlsx")
################################################################
library(plyr)
library(dplyr)
library(data.table)

table(dat$Category)
table(dat$Score)
nrow(dat)
###############################################################
#Training data




#For Category
dat1<-dat # score
dat2<-dat[!is.na(dat$Category),]
# considering data with abtract only
table(is.na(dat2$abstract)) 
# FALSE  TRUE 
# 2342   426 
dat2<-dat2[!is.na(dat2$abstract),]
##############################################################
dat1<-data.frame("Text"=paste(dat1$Title,dat1$abstract),"Score"=dat1$Score)
dat2<-data.frame("Text"=paste(dat2$Title,dat2$abstract),"Category"=dat2$Category)

dat1$Text<-unlist(lapply(dat1$Text,clean_text))
dat2$Text<-unlist(lapply(dat2$Text,clean_text))
###############################################################
# Identify non ascii strings or non english character 
ind<-lapply(dat$Title,function(x){grepl("[[:cntrl:]]", stringi::stri_enc_toascii(x))})
ind<-unlist(ind)
dat1<-dat1[!ind,]  

###############################################################
###############################################################
#stats
#dat1
table(dat1$Score)
nrow(dat1)

table(dat2$Category)
nrow(dat2)

################################################################
#   # Data preparation
# set.seed(777)
# # divide data set into training and test sets
# tr_prop = 0.8    # proportion of full dataset to use for training
# bbc_train = plyr::ddply(BBC, .(Category), function(., seed) { set.seed(seed); .[sample(1:nrow(.), trunc(nrow(.) * tr_prop)), ] }, seed = 101)
# bbc_test = ddply(BBC, .(Category), function(., seed) { set.seed(seed); .[-sample(1:nrow(.), trunc(nrow(.) * tr_prop)), ] }, seed = 101)
# 
# # check that proportions are equal across datasets
# ddply(BBC, .(Category), function(.) nrow(.)/nrow(BBC) )
# ddply(bbc_train, .(Category), function(.) nrow(.)/nrow(bbc_train) )
# ddply(bbc_test, .(Category), function(.) nrow(.)/nrow(bbc_test) )
# #Proporstion
# data.frame("Training Set"=nrow(bbc_train),"Test Set"= nrow(bbc_test),"BBC"= nrow(BBC)) # lengths of se

save(list = c("dat","dat1","dat2"),file = "output/processed.RData")
rm(list = ls())
