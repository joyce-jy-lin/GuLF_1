setwd("/Users/joycelin/Desktop")
library(sas7bdat)
library(tidyverse)
library(ggplot2)
library(broom)       # for tidy model output using tidy() function
library(Hmisc) 
library(gtsummary)
library(summarytools)
 
##load original data and convert to csv
dat<-read.sas7bdat("lin_20230130_deidentified.sas7bdat", debug=TRUE)
datblood<-read.sas7bdat("lin_btex_deidentified_20230130.sas7bdat", debug=TRUE)
datsum<-read.sas7bdat("master_formats.sas7bdat", debug=TRUE)
geodat<-read.sas7bdat("lin_20220512_geo_deidentified.sas7bdat", debug=TRUE)
write.csv(dat, file = "GulfDatanew.csv")
write.csv(datsum, file = "GulfData_masterformat.csv")
write.csv(geodat, file = "GulfGeoDat_.csv")
write.csv(datblood, file = "btexblood.csv")

dat<-read.sas7bdat("lin_20221006_deidentified.sas7bdat", debug=TRUE)
write.csv(dat, file = "GulfData.csv")

link<-read.sas7bdat("lin_idlink.sas7bdat", debug=TRUE)
write.csv(link, file = "IDlink.csv")

## Load data from CSV
dat <- read_excel("GulfData.xlsx")
datsum <- read.csv(file = "GulfData_masterformat.csv")
geodat <- read.csv(file="GulfGeoDat.csv")

##Load data from xlsx with multiple sheets (original and toenail metal participants)
library(tidyverse)
library(dplyr)
library(readxl)
dat <- read_excel("CEtoenail_GulfData_batchLODdivSQRT22.xlsx")
head(dat)

##reformat dates
dat$HVCOMPLETEDATE = format(as.Date(dat$HVCOMPLETEDATE, origin="1960-01-01"),"%Y-%m-%d")
dat$EXAM_COMP_DATE = format(as.Date(dat$EXAM_COMP_DATE, origin="1960-01-01"),"%Y-%m-%d")
dat <- mutate(dat, HVDate = format(as.Date(dat$HVCOMPLETEDATE, origin="1960-01-01"),"%Y-%m-%d"))
dat <- mutate(dat, CEDate = format(as.Date(dat$EXAM_COMP_DATE, origin="1960-01-01"),"%Y-%m-%d"))
dat$HVDate
dat$CEDate

medcoldate = median(dat$EXAM_COMP_DATE)
medcoldate = format(as.Date(medcoldate, origin="1960-01-01"), "%Y-%m-%d")
medcoldate

maxcoldate = max(dat$EXAM_COMP_DATE)
maxcoldate = format(as.Date(maxcoldate, origin="1960-01-01"), "%Y-%m-%d")
maxcoldate

mincoldate = min(dat$EXAM_COMP_DATE)
mincoldate = format(as.Date(mincoldate, origin="1960-01-01"), "%Y-%m-%d")
mincoldate

## Age groups
describe(dat$HV_AGE)
summary(dat$HV_AGE)
summary(dat$CE_AGE)
SDage = sd(dat$CE_AGE)

dat <- dat %>% mutate(agegroup = case_when(HV_AGE >= 70 ~ '6',
                                           HV_AGE >= 60 & HV_AGE <=69 ~ '5',
                                           HV_AGE >= 50 & HV_AGE <=59 ~ '4',
                                           HV_AGE >= 40 & HV_AGE<= 49 ~ '3',
                                           HV_AGE >= 20  & HV_AGE <= 39 ~ '2',
                                           HV_AGE < 20 ~ '1')) # end function
summaryage <- dat %>%
  group_by(agegroup) %>%
  summarise(Count = n())
summaryage

dat <- dat %>% mutate(CEagegroup = case_when(CE_AGE >= 70 ~ '6',
                                           CE_AGE >= 60 & CE_AGE <=69 ~ '5',
                                           CE_AGE >= 50 & CE_AGE <=59 ~ '4',
                                           CE_AGE >= 40 & CE_AGE<= 49 ~ '3',
                                           CE_AGE >= 20 & CE_AGE <= 39 ~ '2',
                                           CE_AGE < 20 ~ '1')) # end function
summaryCEage <- dat %>%
  group_by(CEagegroup) %>%
  summarise(Count = n())
summaryCEage


##time between visits
dat <- dat %>% mutate(timebetweenvisits = difftime(dat$EXAM_COMP_DATE ,dat$HVCOMPLETEDATE , units = c("days")))
describe(dat$timebetweenvisits)
mean(dat$timebetweenvisits)
median(dat$timebetweenvisits)

#### Race/ethnicity summary
describe(dat$EN_RACE2)
summaryrace <- dat %>%
  group_by(EN_RACE2) %>%
  summarise(Count = n())
summaryrace

### State summary
summarystate <- dat %>%
  group_by(EN_STATE) %>%
  summarise(Count = n())
summarystate

describe(dat$EN_EDU)


dfSummary(dat$HV_EMPLOY)
describe(dat$HV_CUPTOHV2)
freq(dat$HV_CUPTOHV2)
describe(dat$HV_CUPTOHV2)
summary(dat$HV_CUPTOHV2)

dfSummary(dat$HV_OILRESID)
dfSummary(dat$HV_P2W_NUM)
## hobbies
dfSummary(dat$HV_K1C_YN)
dfSummary(dat$HV_K1E_YN)

dfSummary(dat$EN_SPILLWKDUR2)
dfSummary(dat$EXP_DUR)
dfSummary(dat$THC_CUMULATIVE1)

hist(dat$EN_SPILLWKDUR2)

dat$EN_RACE2 <- factor(dat$EN_RACE2, 
                   levels = c(1, 2, 3, 4, 9),
                  labels = c("White", "Black", "Asian", "Other", "Multiracial"))
summary(dat$EN_RACE2)
dat$EN_EDU <- as.label(dat$EN_EDU)
fct_recode(dat$EN_EDU,
                              4 == "< Highschool",
                              6 == "< Highschool",
                              7 == "< Highschool",
                              8 == "< Highschool",
                              9 == "< Highschool",
                             10 == "< Highschool",
                             11 =="< Highschool",
                             12 == "< Highschool",
                             13 == "Highschool or Equivalent",
                             14 == "Highschool or Equivalent",
                             15 == "Some College",
                             16 == "Some College",
                             17 == "Some College",
                             18 == "> College Graduate",
                             19 == "> College Graduate",
                             20 == "> College Graduate",
                             21 == "> College Graduate") %>% count(dat$EN_EDU)


Less_HS<-filter(dat, EN_EDU <"13")
Less_HS

tabulate(dat$EN_EDU)
dat%>% count(EN_EDU)

dat%>% count(dat$EN_TOTINCOME)

summary(as.factor(dat$EN_HISPANIC2))



summary(dat$THC_CUMULATIVE1)
dat$CE_A1_DATE <- as.Date(dat$CE_A1_DATE, '1960-01-01')
dat$CE_A1_DATE



## OCCUPATIONAL INFO
## work organization (RCDS)
describe(dat$RCDS)
summaryrcds <- dat %>%
  group_by(RCDS) %>%
  summarise(Count = n())
summaryrcds

## time worked until toenail collection
dat$EN_STARTDATE2 = format(as.Date(dat$EN_STARTDATE2, origin="1960-01-01"),"%Y-%m-%d")
dat$EN_ENDDATE2 = format(as.Date(dat$EN_ENDDATE2, origin="1960-01-01"),"%Y-%m-%d")
dat <- mutate(dat, StartDate = format(as.Date(dat$EN_STARTDATE2, origin="1960-01-01"),"%Y-%m-%d"))
dat <- mutate(dat, EndDate = format(as.Date(dat$EN_ENDDATE2, origin="1960-01-01"),"%Y-%m-%d"))
dat$EN_STARTDATE2
dat$EN_ENDDATE2

dat <- dat %>% mutate(timeworktoenail = difftime(StartDate ,EndDate , units = c("days")))
mean(dat$timeworktoenail)
median(dat$timeworktoenail)
range(dat$timeworktoenail)


##THC level
## work organization (RCDS)
describe(dat$Max_Ordinal_THC_Level)
summaryTHC <- dat %>%
  group_by(Max_Ordinal_THC_Level) %>%
  summarise(Count = n())
summaryTHC

describe(dat$EN_BURN2)


# Blood and Toenail metal correlation -------------------------------------
dat <- read_excel("GulfData_bloodbtex.xlsx")
dat<-dat[!grepl("NA", dat$BX_BLD_BPB_NUM),]
Pb<-log(dat$Pb)
BLDPb<-log(dat$BX_BLD_BPB_NUM)
cor(Pb, BLDPb,  method = "pearson", use = "complete.obs")

Pb<-cor(dat$Pb, dat$BX_BLD_BPB_NUM,  method = "spearman", use = "complete.obs")
Hg<-cor(dat$Hg, dat$BX_BLD_THG_NUM,  method = "spearman", use = "complete.obs")
Mn<-cor(dat$Mn, dat$BX_BLD_BMN_NUM,  method = "spearman", use = "complete.obs")
Se<-cor(dat$Se, dat$BX_BLD_BSE_NUM,  method = "spearman", use = "complete.obs")
Cd<-cor(dat$Cd, dat$BX_BLD_BCD_NUM,  method = "spearman", use = "complete.obs")


# PCA for toenail metals --------------------------------------------------
library(readxl)
dat <- read_excel("GulfData_perfnerv.xlsx")

dftoe <- data_log[, 6:23]
dftoe <- subset(dftoe, select = -c(Cd, Co, V, Mo, Sb))

toepc <- prcomp(dftoe, center = TRUE, scale. = TRUE)
summary(toepc)
toepc

# biplot
biplot(toepc, scale = 0)
eigenvals <- toepc$sdev^2
eigenvals # 2 PCs based off eigenvalues >1

## varimax rotation
toepcvar<- varimax(toepc$rotation)
toepcvar

#promax rotation
toepcpro<-promax(toepc$rotation)
toepcpro

## promax rotation
library(psych)
toepc.pro <-principal(dftoe, nfactors=3, rotate = "promax")
toepc.pro

## varimax rotation
toepc.var <-principal(dftoe, nfactors=2, rotate = "varimax")
toepc.var

## 

###### spearmans correlation between metals------------------------
library("Hmisc")
library(ggplot2)
library(reshape2)

dat <- read_excel("CE_blockgroupNEI.xlsx")
metaldat <- dat[, c(6:23)]
# rearrange columns grouping essential together and toxic together
metaldat1<- dat[, c(8, 15, 12, 6, 11, 18, 16, 7, 17, 10, 22, 14, 23)]

cormat <- cor(metaldat1, method = c("spearman"), use = "complete.obs")
cormat <- round(cormat, 2)
cormat

melted_cormat <- melt(cormat)
head(melted_cormat)

plot <-ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(0,1), space = "Lab", 
                       name="Correlation\n") +
  theme(legend.title=element_text(size=11), legend.text = element_text(size=10)) + geom_text(aes(label = value), color = "black", size = 3) + coord_fixed()

plot

png("corrplot.png", width = 6, height = 6, units = 'in', res = 300) 
plot
dev.off() 

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)

plot<-ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman's\nCorrelation") +
  theme_minimal() + geom_text(aes(label = value), color = "black", size = 2) + coord_fixed()

plot

png("corrplot.png", width = 6, height = 6, units = 'in', res = 300) 
plot
dev.off() 

## Merge data sheets by ID
setwd("/Users/joycelin/Desktop/Gulf/Aim3")
data<- read_excel("CE_neuroscaled_quantiles.xlsx")
diab <- read_excel("Gulfdiabetes.xlsx")

data_merge <- merge(data, diab, by = c("Record", "Record")) 
library(dplyr)
data_merge <- data_merge %>% rename("diabetesdiag" = "CE_C15H1_YN.y")
table(data_merge$diabetesdiag)

data_merge <- data_merge %>% mutate(diabetesdiag = case_when(diabetesdiag == 1 ~ 1,
                                                           diabetesdiag == 2 ~ 0,
                                                           diabetesdiag == 8 ~ 0))
write.csv(data_merge, file = "CE_neuroscaled_quantiles.csv")
