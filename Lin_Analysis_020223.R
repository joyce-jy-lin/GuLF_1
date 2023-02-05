## 10/21/22
## GuLF Toenail Metals Analysis - Low mass toenail reliability study (Paper 1)
setwd("/Users/joycelin/Desktop/Gulf/Aim1")
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

# Testing log transformation effect on normality

Al<-ggplot(data=CE, aes(x=Al)) + geom_histogram(bins=30)+ ggtitle("Aluminum") + xlab("(µg/g)") + ylab("Count")
As<-ggplot(data=CE, aes(x=As)) + geom_histogram(bins=30)+ ggtitle("Arsenic") + xlab("(µg/g)") + ylab("Count")
Ca<-ggplot(data=CE, aes(x=Ca)) + geom_histogram(bins=30)+ ggtitle("Calcium") + xlab("(µg/g)") + ylab("Count")
Cr<-ggplot(data=CE, aes(x=Cr)) + geom_histogram(bins=30)+ ggtitle("Chromium") + xlab("(µg/g)") + ylab("Count")
Cu<-ggplot(data=CE, aes(x=Cu)) + geom_histogram(bins=30)+ ggtitle("Copper") + xlab("(µg/g)") + ylab("Count")
Fe<-ggplot(data=CE, aes(x=Fe)) + geom_histogram(bins=30)+ ggtitle("Iron") + xlab("(µg/g)") + ylab("Count")
Hg<-ggplot(data=CE, aes(x=Hg)) + geom_histogram(bins=30)+ ggtitle("Mercury") + xlab("(µg/g)") + ylab("Count")
Mg<-ggplot(data=CE, aes(x=Mg)) + geom_histogram(bins=30)+ ggtitle("Magnesium") + xlab("(µg/g)") + ylab("Count")
Mn<-ggplot(data=CE, aes(x=Mn)) + geom_histogram(bins=30)+ ggtitle("Manganese") + xlab("(µg/g)") + ylab("Count")
Ni<-ggplot(data=CE, aes(x=Ni)) + geom_histogram(bins=30)+ ggtitle("Nickel") + xlab("(µg/g)") + ylab("Count")
Pb<-ggplot(data=CE, aes(x=Pb)) + geom_histogram(bins=30)+ ggtitle("Lead") + xlab("(µg/g)") + ylab("Count")
Se<-ggplot(data=CE, aes(x=Se)) + geom_histogram(bins=30)+ ggtitle("Selenium") + xlab("(µg/g)") + ylab("Count")
Zn<-ggplot(data=CE, aes(x=Al)) + geom_histogram(bins=30)+ ggtitle("Zinc") + xlab("(µg/g)") + ylab("Count")

library(patchwork) 
figure<- Al + As +Ca +Cr + Cu + Fe+ Hg + Mg + Mn + Ni + Pb + Se + Zn + plot_layout(ncol = 4)
figure
figure <-figure + plot_annotation(title = 'A') 
png("fig.png", width = 10, height = 13, units = 'in', res = 300) 
figure
dev.off()
figure

xl <- expression(Log[10](µg/g))
Allog<-ggplot(data=CE, aes(x=log10(Al))) + geom_histogram(bins=30)+ ggtitle("Aluminum") + xlab(xl)+ ylab("Count")
Aslog<-ggplot(data=CE, aes(x=log10(As))) + geom_histogram(bins=30)+ ggtitle("Arsenic") + xlab(xl) + ylab("Count")
Calog<-ggplot(data=CE, aes(x=log10(Ca))) + geom_histogram(bins=30)+ ggtitle("Calcium") + xlab(xl) + ylab("Count")
Crlog<-ggplot(data=CE, aes(x=log10(Cr))) + geom_histogram(bins=30)+ ggtitle("Chromium") + xlab(xl) + ylab("Count")
Culog<-ggplot(data=CE, aes(x=log10(Cu))) + geom_histogram(bins=30)+ ggtitle("Copper") + xlab(xl) + ylab("Count")
Felog<-ggplot(data=CE, aes(x=log10(Fe))) + geom_histogram(bins=30)+ ggtitle("Iron") + xlab(xl) + ylab("Count")
Hglog<-ggplot(data=CE, aes(x=log10(Hg))) + geom_histogram(bins=30)+ ggtitle("Mercury") + xlab(xl) + ylab("Count")
Mglog<-ggplot(data=CE, aes(x=log10(Mg))) + geom_histogram(bins=30)+ ggtitle("Magnesium") + xlab(xl) + ylab("Count")
Mnlog<-ggplot(data=CE, aes(x=log10(Mn))) + geom_histogram(bins=30)+ ggtitle("Manganese") + xlab(xl) + ylab("Count")
Nilog<-ggplot(data=CE, aes(x=log10(Ni))) + geom_histogram(bins=30)+ ggtitle("Nickel") + xlab(xl) + ylab("Count")
Pblog<-ggplot(data=CE, aes(x=log10(Pb))) + geom_histogram(bins=30)+ ggtitle("Lead") + xlab(xl) + ylab("Count")
Selog<-ggplot(data=CE, aes(x=log10(Se))) + geom_histogram(bins=30)+ ggtitle("Selenium") + xlab(xl) + ylab("Count")
Znlog<-ggplot(data=CE, aes(x=log10(Al))) + geom_histogram(bins=30)+ ggtitle("Zinc") + xlab(xl) + ylab("Count")

figure1<- Allog + Aslog +Calog +Crlog + Culog + Felog + Hglog + Mglog + Mnlog + Nilog + Pblog + Selog + Znlog + plot_layout(ncol = 4)
figure1
figure1 <-figure1 + plot_annotation(title = 'B') 
png("fig1.png", width = 10, height = 13, units = 'in', res = 300) 
figure1
dev.off()
figure1

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

# metals above 60% detection
low_metals <- c("Cd", "Co", "Mo" ,"V", "Sb") # metals below 60% detect
`%notin%` <- Negate(`%in%`)

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
data <- read_excel("ICC_FINAL_LK.xlsx")
# run these functions to allow for the equal x,y axis in the faceted plots 
FacetEqualWrap <- ggproto(
  "FacetEqualWrap", FacetWrap,
  
  train_scales = function(self, x_scales, y_scales, layout, data, params) {
    
    # doesn't make sense if there is not an x *and* y scale
    if (is.null(x_scales) || is.null(x_scales)) {
      stop("X and Y scales required for facet_equal_wrap")
    }
    
    # regular training of scales
    ggproto_parent(FacetWrap, self)$train_scales(x_scales, y_scales, layout, data, params)
    
    # switched training of scales (x and y and y on x)
    for (layer_data in data) {
      match_id <- match(layer_data$PANEL, layout$PANEL)
      
      x_vars <- intersect(x_scales[[1]]$aesthetics, names(layer_data))
      y_vars <- intersect(y_scales[[1]]$aesthetics, names(layer_data))
      
      SCALE_X <- layout$SCALE_X[match_id]
      ggplot2:::scale_apply(layer_data, y_vars, "train", SCALE_X, x_scales)
      
      SCALE_Y <- layout$SCALE_Y[match_id]
      ggplot2:::scale_apply(layer_data, x_vars, "train", SCALE_Y, y_scales)
    }
    
  }
)
facet_wrap_equal <- function(...) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)
  
  ggproto(NULL, FacetEqualWrap,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}


low_metals <- c("Cd", "Co", "Mo" ,"V", "Sb") # metals below 60% detect
`%notin%` <- Negate(`%in%`)

repeated_measures <- data %>% pivot_longer(
  cols = Mg:Pb_LOD
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

LOD_data <- repeated_measures %>%rowwise()%>% mutate(
  cond = length(str_split(name, "_")[[1]])
) %>% filter(cond == 2) %>% mutate(
  Metal = str_split(name, "_")[[1]][1]
) %>% dplyr::select(-c(name, cond))

repeated_measures <- repeated_measures %>%rowwise()%>% mutate(
  cond = length(str_split(name, "_")[[1]])
) %>% filter(cond == 1) %>% dplyr::select(-cond)

# create indicator about whether samples 1 or 2, 2 or 3, or 1 or 3 are below LOD 
LOD_data <- LOD_data %>% rowwise() %>% mutate(
  `1|2` = ifelse(`1`==1|`2`==1, 1, 0),
  `1|3` = ifelse(`1`==1|`3`==1, 1, 0),
  `2|3` = ifelse(`1`==1|`3`==1, 1, 0)
)

colnames(repeated_measures) <- c("ID", "Metal", "Sample1", "Sample2", "Sample3")
repeated_measures <- repeated_measures %>% left_join(., LOD_data %>% 
                                                       dplyr::select(ID, Metal, `1|2`, `2|3`, `1|3`),
                                                     by = c("ID", "Metal"))


# Supplemental figure S1. scatterplots of repeated samples 1 vs 2, 2 vs 3, 1 vs 3

# UPDATED equal scales and shape denoting limit of detection 
repeated_measures %>% mutate(Sample1 = log10(Sample1), Sample2 = log10(Sample2)) %>% 
  filter(Metal %notin% low_metals) %>% ggplot(aes(x = Sample1, y = Sample2, shape = factor(`1|2`)))+geom_point()+theme_minimal()+
  facet_wrap_equal(.~Metal, scales="free")+geom_abline(slope=1, alpha=.5, color="grey")+labs(
    title = "Repeated Samples: 1 vs. 2", x = "Sample 1", y = "Sample 2")+
  scale_shape_discrete(name = "", labels = c("Both Samples Above LOD", "At Least One Sample Below LOD"))+
  theme(legend.position="bottom")

repeated_measures %>%mutate(Sample2 = log10(Sample2), Sample3 = log10(Sample3))%>% filter(!is.na(Sample3))%>%
  filter(Metal %notin% low_metals) %>% ggplot(aes(x = Sample2, y = Sample3, shape= factor(`2|3`)))+geom_point()+theme_minimal()+
  facet_wrap_equal(.~Metal, scales="free")+geom_abline(slope=1, alpha=.5, color="grey")+labs(
    title = "Repeated Samples: 2 vs. 3", x = "Sample 2", y = "Sample 3")+
  scale_shape_discrete(name = "", labels = c("Both Samples Above LOD", "At Least One Sample Below LOD"))+
  theme(legend.position="bottom")

repeated_measures %>%mutate(Sample1 = log10(Sample1), Sample3 = log10(Sample3))%>% filter(!is.na(Sample3)) %>%
  filter(Metal %notin% low_metals) %>% ggplot(aes(x = Sample1, y = Sample3,shape=factor(`1|3`)))+geom_point()+theme_minimal()+
  facet_wrap_equal(.~Metal, scales="free")+geom_abline(slope=1, alpha=.5, color="grey")+labs(
    title = "Repeated Samples: 1 vs. 3", x = "Sample 1", y = "Sample 3")+
  scale_shape_discrete(name = "", labels = c("Both Samples Above LOD", "At Least One Sample Below LOD"))+
  theme(legend.position="bottom")

metals <- unique(repeated_measures$Metal)

# Figure 2. Scatterplots of repeated measures (1v2, 2v3, 1v3) for Cu and Cr
library(patchwork)
CuCr <- filter(repeated_measures, Metal == "Cu"| Metal =="Cr")
CuCr_dat <-CuCr %>%mutate(Sample1 = log10(Sample1), Sample2 = log10(Sample2), Sample3 = log10(Sample3))


fig1_updated <- ggplot(CuCr_dat,aes(x = Sample1, y = Sample2, color = Metal))+ 
  scale_color_manual(values=c("#FC9C00", "#2D9503"))+ geom_point(aes(shape=factor(`1|2`), color=Metal, size=3))+
  theme_minimal()+ theme(legend.position="none") +
  geom_abline(slope=1, alpha=.5, color="grey", size = 1) +xlab("Sub-sample 1 (log(μg/g))") + 
  ylab("Sub-sample 2 (log(μg/g))") + 
  theme(axis.title = element_text(size = 13, face = "bold"), axis.text = element_text(size=13))+
  scale_x_continuous(limits=c(-4.2, 1.3),breaks=seq(-4,1,1))+
  scale_y_continuous(limits=c(-4.2, 1.3), breaks=seq(-4, 1, 1))


fig1_updated


fig2_updated <-ggplot(CuCr_dat,aes(x = Sample2, y = Sample3, color = Metal))+
  scale_color_manual(values=c("#FC9C00", "#2D9503"))+geom_point(aes(shape=factor(`2|3`), color=Metal, size=3))+
  theme_minimal()+geom_abline(slope=1, alpha=.5, color="grey", size = 1) +
  xlab("Sub-sample 2 (log(μg/g))") + ylab("Sub-sample 3 (log(μg/g))") + 
  theme(axis.title = element_text(size = 13, face = "bold"), axis.text = element_text(size=13))+
  guides(color = guide_legend(override.aes = list(size=13)))+
  scale_x_continuous(limits=c(-4.2, 1.3),breaks=seq(-4,1,1))+
  scale_y_continuous(limits=c(-4.2, 1.3), breaks=seq(-4, 1, 1))+
  theme(legend.position="none") 

fig2_updated

fig3_updated<-ggplot(CuCr_dat,aes(x = Sample1, y = Sample3, color = Metal))+
  scale_color_manual(values=c("#FC9C00", "#2D9503"))+geom_point(aes(shape=factor(`1|3`), color=Metal, size=3))+theme_minimal()+theme(legend.position="none")+geom_abline(slope=1, alpha=.5, color="grey", size = 1)+
  xlab("Sub-sample 1 (log(μg/g))")+ 
  ylab("Sub-sample 3 (log(μg/g))") +theme(axis.title = element_text(size = 13, face = "bold"), axis.text = element_text(size=13))+
  scale_x_continuous(limits=c(-4.2, 1.3),breaks=seq(-4,1,1))+
  scale_y_continuous(limits=c(-4.2, 1.3), breaks=seq(-4, 1, 1))


fig3_updated

figure<- fig1_updated + fig3_updated + fig2_updated
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

