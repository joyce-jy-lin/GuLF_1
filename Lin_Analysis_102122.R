## Joyce Lin
## 10/21/22
## GuLF Toenail Metals Analysis - Low mass toenail reliability study (Paper 1)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(readxl)
library(lme4)
library(wPerm)
library(irr)
library(gt)
library(psych)
library(EnvStats)
library(paletteer)
library(devtools)
library(sjstats)
library(DescTools)
library(rcompanion)

# Summary stats comparing metal concentrations at visit 1 vs visit 2 --------
# import data
data <- read_excel("Final_Spearman.xlsx", skip = 1)
data$Visit <- factor(data$Visit, levels = 1:2, labels = c("HV", "CE"))
# remove outliers subjects 94 & 208
data <- data %>% mutate(ID = as.numeric(ID)) %>% filter(!is.na(ID) & ID != 94 & ID != 208)

# get summary stats for concentrations by metal by visit 
HV<- filter(data, Visit == "HV")
CE<- filter(data, Visit=="CE")
median(HV$Mg)
median(CE$Mg)
median(HV$Al)
median(CE$Al)

# Table 4 summary stats for change in median metal concentration over time 
Mg <-data%>% dplyr::select(ID, Visit, Mg)
Mg<-Mg%>% pivot_wider(names_from = Visit, values_from = Mg)
Mg_<- mutate(Mg,change = CE-HV)
medchange_Mg <- median(Mg_$change)

Mn <-data %>% dplyr::select(ID,Visit, Mn)
Mn <-Mn%>% pivot_wider(names_from = Visit, values_from = Mn)
Mn <- mutate(Mn,change = CE-HV)
medchange_Mn <- median(Mn$change)

Al <-data %>% dplyr::select(ID,Visit, Al)
Al <-Al%>% pivot_wider(names_from = Visit, values_from = Al)
Al <- mutate(Al,change = CE-HV)
medchange_Al <- median(Al$change)

As <-data %>% dplyr::select(ID,Visit, As)
As <-As%>% pivot_wider(names_from = Visit, values_from = As)
As <- mutate(As,change = CE-HV)
medchange_As <- median(As$change)

Ca <-data %>% dplyr::select(ID,Visit, Ca)
Ca <-Ca%>% pivot_wider(names_from = Visit, values_from = Ca)
Ca <- mutate(Ca,change = CE-HV)
medchange_Ca <- median(Ca$change)

Cr <-data %>% dplyr::select(ID,Visit, Cr)
Cr <-Cr%>% pivot_wider(names_from = Visit, values_from = Cr)
Cr <- mutate(Cr,change = CE-HV)
medchange_Cr <- median(Cr$change)

Cu <-data %>% dplyr::select(ID,Visit, Cu)
Cu <-Cu%>% pivot_wider(names_from = Visit, values_from = Cu)
Cu <- mutate(Cu,change = CE-HV)
medchange_Cu <- median(Cu$change)

Fe <-data %>% dplyr::select(ID,Visit, Fe)
Fe <-Fe%>% pivot_wider(names_from = Visit, values_from = Fe)
Fe <- mutate(Fe,change = CE-HV)
medchange_Fe <- median(Fe$change)

Pb <-data %>% dplyr::select(ID,Visit, Pb)
Pb <-Pb%>% pivot_wider(names_from = Visit, values_from = Pb)
Pb <- mutate(Pb,change = CE-HV)
medchange_Pb <- median(Pb$change)

Hg <-data %>% dplyr::select(ID,Visit, Hg)
Hg <-Hg%>% pivot_wider(names_from = Visit, values_from = Hg)
Hg <- mutate(Hg,change = CE-HV)
medchange_Hg <- median(Hg$change)

Ni <-data %>% dplyr::select(ID,Visit, Ni)
Ni <-Ni%>% pivot_wider(names_from = Visit, values_from = Ni)
Ni <- mutate(Ni,change = CE-HV)
medchange_Ni <- median(Ni$change)

Se <-data %>% dplyr::select(ID,Visit, Se)
Se <-Se%>% pivot_wider(names_from = Visit, values_from = Se)
Se <- mutate(Se,change = CE-HV)
medchange_Se <- median(Se$change)

Zn <-data %>% dplyr::select(ID,Visit, Zn)
Zn <-Zn%>% pivot_wider(names_from = Visit, values_from = Zn)
Zn <- mutate(Zn,change = CE-HV)
medchange_Zn <- median(Zn$change)

# Descriptive summary plots comparing metal concentrations at visit 1 vs visit 2 --------------------
# log transform each concentration 
data_log <- data %>% mutate(across(Mg:Pb, log10))

# Supplemental Figure S2 - density plots of log metal concentration by visit separated by metals above and below 60% detect (supplemental figure S2)
col1 <-"#3333FF"; col2 <- "#CBFFCB"
col <- c("HV"= "#3333FF", "CE" = "#65FF65")

low_metals <- c("Cd", "Co", "Mo" ,"V", "Sb") # metals below 60% detect
`%notin%` <- Negate(`%in%`)

# metals above 60% detection
data_log %>% pivot_longer(cols = Mg:Pb) %>% filter(name %notin% low_metals) %>% ggplot(aes(x = value, color = Visit, fill = Visit)) +
  geom_density(alpha = .7) + facet_wrap(. ~ name, scales = "free") + theme_minimal() +
  scale_color_manual(name = "", labels = c("HV (n=123)", "CE (n=123)"), values = col) +
  scale_fill_manual(name = "", labels = c("HV (n=123)", "CE (n=123)"), values=col)+ labs(x =expression( "Log10 Concentration" ~ mu*g/g), y = "Density", title = "Concentrations by Metal and Visit")+
  theme(legend.position="bottom")

# metals below 60% detection
data_log %>% pivot_longer(cols = Mg:Pb) %>% filter(name %in% low_metals) %>% ggplot(aes(x = value, color = Visit, fill = Visit)) +
  geom_density(alpha = .7) + facet_wrap(. ~ name, scales = "free") + theme_minimal() +
  scale_color_manual(name = "", labels = c("HV (n=123)", "CE (n=123)"), values = col) +
  scale_fill_manual(name = "", labels = c("HV (n=123)", "CE (n=123)"), values=col)+ labs(x =expression( "Log10 Concentration" ~ mu*g/g), y = "Density", title = "Concentrations by Metal and Visit")+
  theme(legend.position="bottom")

# scatterplots of log metal concentrations by visit for metals above 60% detect 
data_log %>% dplyr::select(-Batch) %>% pivot_longer(cols = Mg:Pb) %>% dplyr::select(ID, name, Visit, value) %>% pivot_wider(values_from= "value", names_from = Visit) %>% filter(name %notin% low_metals) %>%
  ggplot(aes(x = HV, y = CE))+geom_point(alpha=.5)+facet_wrap(.~name, scales="free")+theme_minimal()+geom_abline(slope = 1, alpha=.7, color= "grey")+
  labs(title = "Scatterplot of Log10 Concentrations by Visit")

# scatterplots of log metal concentrations by visit for metals below 60% detect 
data_log %>% dplyr::select(-Batch) %>% pivot_longer(cols = Mg:Pb) %>% dplyr::select(ID, name, Visit, value) %>% pivot_wider(values_from= "value", names_from = Visit) %>% filter(name %in% low_metals) %>%
  ggplot(aes(x = HV, y = CE))+geom_point(alpha=.5)+facet_wrap(.~name, scales="free")+theme_minimal()+geom_abline(slope = 1, alpha=.7, color= "grey")+
  labs(title = "Scatterplot of Log10 Concentrations by Visit", subtitle = "Below LOD Metals")

# Table 4 Spearmans correlation calculations --------------------------------------
data_median_log <- data_log %>% group_by(ID, Visit) %>% summarize(
  across(Mg:Pb, median))

data_median <- data %>% group_by(ID, Visit) %>% summarize(
  across(Mg:Pb, median))
results <- matrix(NA, nrow = 18, ncol = 9)
for(i in 1:18){
  metal <-colnames(data_median_log)[i+2]
  data_median_metal <- data_median %>% dplyr::select(c(metal, ID, Visit)) %>%
    pivot_wider(
      id_cols = ID,
      names_from = Visit,
      values_from = metal)  %>% filter(!is.na(CE) & !is.na(HV))
  mean <- mean(c(data_median_metal$HV, data_median_metal$CE))
  median <- median(c(data_median_metal$HV, data_median_metal$CE))
  gsd <- geoSD(c(data_median_metal$HV, data_median_metal$CE))
  # get spearman's correlation and pvals, CIs from permutation 
  res <- perm.relation(data_median_metal$HV, data_median_metal$CE, "spearman")
  pv <- res$p.value
  spcor <- res$Observed
  se_cor <- sd(res$Perm.values)
  lower_cor <- spcor - (1.96*se_cor)
  upper_cor <- spcor + (1.96*se_cor)
  # store results 
  results[i, 1] <- metal 
  results[i, 2] <- mean
  results[i,3] <- median
  results[i,4] <- gsd
  results[i,5] <- spcor
  results[i,6] <- pv
  results[i,7] <- lower_cor
  results[i,8] <- upper_cor
  results[i,9] <- se_cor
}

resultsdf <- data.frame(results)

colnames(resultsdf) <- c("Metal", "Mean", "Median", "GSD",  
                         "Cor", "Cor Pval", "LB Cor", "UB Cor", "SD Cor") 
resultsdf <- resultsdf %>% mutate(across("Mean":"SD Cor",
                                         as.numeric))

tab1 <- resultsdf %>% select(c("Metal", "Median","GSD", "Cor", "Cor Pval")) %>%  gt() %>% 
  cols_label(Cor = "Spearman Cor", "Cor Pval" = "P Value") %>% fmt_scientific(
    columns = 2:5, decimals = 2
  )
tab1 
tab1 %>% gtsave("tab_1.rtf")



# Summary and plots for repeated measures in 29 participants --------------
data <- read_excel("ICC_FINAL.xlsx")

low_metals <- c("Cd", "Co", "Mo" ,"V", "Sb") # metals below 60% detect
`%notin%` <- Negate(`%in%`)

repeated_measures <- data %>% pivot_longer(
  cols = Mg:Pb
) %>% group_by(ID, name, Visit) %>% mutate(
  n = n()
) %>% mutate(
  max = max(Samplenum),
  samplenum2 = case_when(
    max == n ~ Samplenum,
    TRUE ~ Samplenum - (max-n)
  )) %>% pivot_wider(
    id_cols = c(ID, name),
    names_from = samplenum2
  ) %>% ungroup() 


colnames(repeated_measures) <- c("ID", "Metal", "Sample1", "Sample2", "Sample3")


# Supplemental figure S1. scatterplots of repeated samples 1 vs 2, 2 vs 3, 1 vs 3
repeated_measures %>% mutate(Sample1 = log10(Sample1), Sample2 = log10(Sample2)) %>% filter(Metal %notin% low_metals) %>% ggplot(aes(x = Sample1, y = Sample2))+geom_point()+theme_minimal()+
  facet_wrap(.~Metal, scales="free")+geom_abline(slope=1, alpha=.5, color="grey")+labs(
    title = "Repeated Samples: 1 vs. 2")

repeated_measures %>%mutate(Sample2 = log10(Sample2), Sample3 = log10(Sample3))%>% filter(Metal %notin% low_metals) %>% ggplot(aes(x = Sample2, y = Sample3))+geom_point()+theme_minimal()+
  facet_wrap(.~Metal, scales="free")+geom_abline(slope=1, alpha=.5, color="grey")+labs(
    title = "Repeated Samples: 2 vs. 3")

repeated_measures %>%mutate(Sample1 = log10(Sample1), Sample3 = log10(Sample3))%>% filter(Metal %notin% low_metals) %>% ggplot(aes(x = Sample1, y = Sample3))+geom_point()+theme_minimal()+
  facet_wrap(.~Metal, scales="free")+geom_abline(slope=1, alpha=.5, color="grey")+labs(
    title = "Repeated Samples: 1 vs. 3")

metals <- unique(repeated_measures$Metal)

# Figure 2. Scatterplots of repeated measures (1v2, 2v3, 1v3) for Cu and Cr
library(patchwork)
CuCr <- filter(repeated_measures, Metal == "Cu"| Metal =="Cr")
CuCr_dat <-CuCr %>%mutate(Sample1 = log10(Sample1), Sample2 = log10(Sample2), Sample3 = log10(Sample3))

fig1 <-ggplot(CuCr_dat,aes(x = Sample1, y = Sample2, color = Metal))+ scale_color_manual(values=c("#FC9C00", "#2D9503"))+ geom_point(aes(shape=Metal, color=Metal, size=3))+theme_minimal()+ theme(legend.position="none") +geom_abline(slope=1, alpha=.5, color="grey", size = 1) +xlab("Sub-sample 1 (log(μg/g))") + ylab("Sub-sample 2 (log(μg/g))") + theme(axis.title = element_text(size = 13, face = "bold"), axis.text = element_text(size=13))
fig1

fig2 <-ggplot(CuCr_dat,aes(x = Sample2, y = Sample3, color = Metal))+scale_color_manual(values=c("#FC9C00", "#2D9503"))+geom_point(aes(shape=Metal, color=Metal, size=3))+theme_minimal()+geom_abline(slope=1, alpha=.5, color="grey", size = 1) +xlab("Sub-sample 2 (log(μg/g))") + ylab("Sub-sample 3 (log(μg/g))") + theme(axis.title = element_text(size = 13, face = "bold"), axis.text = element_text(size=13)) +guides(color = guide_legend(override.aes = list(size=13)))
fig2

fig3 <-ggplot(CuCr_dat,aes(x = Sample1, y = Sample3, color = Metal))+scale_color_manual(values=c("#FC9C00", "#2D9503"))+geom_point(aes(shape=Metal, color=Metal, size=3))+theme_minimal()+theme(legend.position="none")+geom_abline(slope=1, alpha=.5, color="grey", size = 1) +xlab("Sub-sample 1 (log(μg/g))") + ylab("Sub-sample 3 (log(μg/g))") + theme(axis.title = element_text(size = 13, face = "bold"), axis.text = element_text(size=13))
fig3

figure<- fig1 + fig3 + fig2
figure
png("figure.png", width = 16, height = 4.5, units = 'in', res = 400) 
figure
dev.off()


# Table 2. calculating Kendall's W and log 10 transformed ICC ----------------------
resultsicc <- matrix(NA, nrow = 18, ncol = 3)
for(i in 1:18){
  metal <- metals[i]
  dfmetal <- repeated_measures %>% filter(Metal == metal) %>% mutate(
    Sample1 = log10(Sample1),
    Sample2 = log10(Sample2),
    Sample3 = log10(Sample3)
  )
  resw <- kendallW(dfmetal[,3:5], correct=T, ci=T, type = "norm")
  icctest <- ICC(dfmetal[,3:5])
  icc_ub <- icctest$results$`upper bound`[1]
  icc_lb <- icctest$results$`lower bound`[1]
  icc <- icctest$results[1,2]
  resultsicc[i, 1] <- metal 
  resultsicc[i,2] <- resw$W
  resultsicc[i,3] <- icc
}

resultsiccdf <- data.frame(resultsicc)


colnames(resultsiccdf) <- c("Metal", "KW", "ICC")

resultsiccdf <- resultsiccdf %>% mutate_at(2:3,
                                           as.numeric)

# create summary table 2
resultsiccdf %>% arrange(., Metal) %>% mutate(diff = KW -ICC) %>% gt() %>%
  cols_label("KW" = "Kendall's W", "diff" = "Difference") %>%
  fmt_number(columns = 2:4, decimals=2)

results_icc_filter <- resultsiccdf %>% filter(Metal %notin% low_metals)

level_order <- resultsiccdf %>% arrange(.,KW) %>% select(Metal) %>% unlist() %>% unname()

# Simulation for section 2.3.2 of manuscript --------------------------------------------------------------

set.seed(123)
nsims <- 10000
nmetals <- metals[metals%notin% low_metals]
results <- matrix(NA, nrow=13, ncol = 5)

repeated_measures <- repeated_measures %>% filter(Metal %notin% low_metals)
for(i in 1:13){
  # log transform data 
  # first calculate untransformed ICC 
  metal <- nmetals[i]
  df <- repeated_measures %>% filter(Metal == nmetals[i]) %>% mutate(
    logSample1 = log10(Sample1),
    logSample2 = log10(Sample2),
    logSample3 = log10(Sample3)
  ) 
  dfscale <- data.frame(scale(df[,6:8])) # making each sample have mean 0 variance 1
  res_raw <- kendallW(df[,3:5]) # calculate kw 
  kw <- res_raw #store
  icc <- ICC(dfscale) # calculating icc 
  iccres <- icc$results[1,2] # getting icc1
  betas <- rep(NA, nsims)
  for(j in 1:nsims){
    dfscale <- dfscale %>% rowwise() %>% mutate(
      x_rand = sample(c(logSample1, logSample2, logSample3), 1),
      x_mean = mean(logSample1, logSample2, logSample3),
      y = x_mean
    )
    
    res <- lm(y~x_rand, data = dfscale)
    betas[j] <- res$coefficients[2]
  }
  beta <- mean(betas)
  se <- sd(betas)
  results[i,] <- c(metal, kw, iccres, beta, se)
}


results <- data.frame(results)
colnames(results) <- c("Metal", "KW", "ICC", "EffectStd", "SEStd")
results <- results %>% mutate_at(2:5, as.numeric)


cols <- c("Ni" = "#0055AAFF" , "Hg" = "#C40003FF",
          "Fe"  = "#00C19BFF", "Cr" ="#EAC862FF", "As"= "#7FD2FFFF",
          "Al"="#007ED3FF", "Ca" ="#B2DF8AFF","Mn"="#FFACAAFF", "Mg"= "#FF9D1EFF",
          "Se"="#C3EF00FF", "Cu" ="#CAB2D6FF","Pb" ="#894FC6FF", "Zn" = "black")


results %>% ggplot(aes(x = KW, y = EffectStd, col = Metal))+geom_point()+theme_minimal() +
  geom_errorbar(aes(ymin=EffectStd-SEStd, ymax=EffectStd+SEStd), width=.01, size=1.1, alpha=.8)+scale_x_continuous(breaks=seq(.65,.95,.05), limits =c(.65,.95))+
  scale_y_continuous(breaks=seq(-.1, 1.2, .1))+scale_color_manual(values=cols, name = "")+
  labs(x ="Kendall's W", y = "Effect Estimate", title = "Kendall's W and Simulated Effect Estimates",
       subtitle = "Standardized Data")+theme(legend.position="bottom")+geom_hline(aes(yintercept=1, col = "black"))

results %>% ggplot(aes(x = ICC, y = EffectStd, col = Metal))+geom_point()+theme_minimal() +
  geom_errorbar(aes(ymin=EffectStd-SEStd, ymax=EffectStd+SEStd), width=.01, size=1.1, alpha=.8)+scale_x_continuous(breaks=seq(.65,.95,.05), limits =c(.65,.95))+
  scale_y_continuous(breaks=seq(-.1, 1.2, .1))+scale_color_manual(values=cols, name = "")+
  labs(x ="ICC", y = "Effect Estimate", title = "ICC and Simulated Effect Estimates",
       subtitle = "Standardized Data")+theme(legend.position="bottom")+geom_hline(aes(yintercept=1, col = "black"))

# Table 3.Attenuation of the association between sub-sample concentration and the mean concentration of 3 repeated sub-samples due to sub-sample variability.
results %>% arrange(., Metal)%>% gt() %>% 
  fmt_number(columns = 2:5, decimals=2) %>% cols_label(KW= "Kendall's W", EffectStd = "Effect Size, Std. Data", SEStd = "SE Effect Size")
