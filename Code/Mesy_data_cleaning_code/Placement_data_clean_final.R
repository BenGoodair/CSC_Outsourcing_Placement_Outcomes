####library list like a loser####
library(dplyr)
library(tidyverse)
library(effects)
library(stringr)
library(grid)
library(cowplot)
library(rebus)
library(plm)
library(ggridges)
library(CBPS)
library(hrbrthemes)
library(pdynmc)
library(clubSandwich)
library(cobalt)
library(cowplot)
library(sp)
library(maptools)
library(sjPlot)
library(sf)
library(jtools)
library(modelsummary)
library(ggalluvial)
library(gt)
library(lme4)
library(ggiraph)
library(ggiraphExtra)
library(panelr)
library(kableExtra)
library(nnet)
library(tidyclm)
library(gtsummary)
library(tidyverse)
library(plyr)
library(MASS)
library(ordinal)
library(ggeffects)
library(stargazer)
library(ggpubr)
library(gridExtra)
library(sjPlot)
library(lubridate)
library(emmeans)

setwd("C:/Users/bengo/OneDrive - Nexus365/Documents/Children's Care Homes Project")
options(scipen=999)


rm(list=setdiff(ls(), c("")))
####start####

cla2011 <- read.csv("Data/SSDA_2011.csv")
cla2012 <- read.csv("Data/SSDA_2012.csv")
cla2013 <- read.csv("Data/SSDA_2013.csv")
cla2014 <- read.csv("Data/SSDA_2014.csv")
cla2015 <- read.csv("Data/SSDA_2015.csv")
cla2016 <- read.csv("Data/SSDA_2016.csv")
cla2017 <- read.csv("Data/SSDA_2017.csv")
cla2021 <- read.csv("Data/2022_characteristics.csv")
miss2015 <- read.csv("Data/SFR34_Missing2015.csv")
miss2016 <- read.csv("Data/SFR41_Missing2016.csv")
miss2017 <- read.csv("Data/SFR50_Missing_2017.csv")

cl2011 <- read.csv("Data/2011_leavers.csv")
cl2012 <- read.csv("Data/2012_leavers.csv")
cl2013 <- read.csv("Data/2013_leavers.csv")
cl2014 <- read.csv("Data/2014_leavers.csv")
cl2015 <- read.csv("Data/2015_leavers.csv")
cl2016 <- read.csv("Data/2016_leavers.csv")
cl2017 <- read.csv("Data/2017_leavers.csv")

cla2021stab <- read.csv("Data/2022_stability.csv")
cla2021miss <- read.csv("Data/2022_missing.csv")
cla2021short <- read.csv("Data/2022_short.csv")
cla2021leaver <- read.csv("Data/la_care_leavers_intouch.csv")
cla2021leaveracc <- read.csv("Data/la_care_leavers_accommodation_suitability.csv")
cla2021convict <- read.csv("Data/la_conviction_health_outcome_cla.csv")

cla2011$year <- "2011"
cla2012$year <- "2012"
cla2013$year <- "2013"
cla2014$year <- "2014"
cla2015$year <- "2015"
cla2016$year <- "2016"
cla2017$year <- "2017"

cl2011$year <- "2011"
cl2012$year <- "2012"
cl2013$year <- "2013"
cl2014$year <- "2014"
cl2015$year <- "2015"
cl2016$year <- "2016"
cl2017$year <- "2017"

miss2015$year <- "2015"
miss2016$year <- "2016"
miss2017$year <- "2017"
miss2017 <- miss2017 %>% dplyr::select(-LA_order)
names(miss2017) <- names(miss2016)
miss <- rbind(miss2015, miss2016, miss2017)

cla2011 <-cla2011[which(cla2011$geog_l=="LA"),]
cla2012 <-cla2012[which(cla2012$geog_l=="LA"),]
cla2013 <-cla2013[which(cla2013$geog_l=="LA"),]
cla2014 <-cla2014[which(cla2014$geog_l=="LA"),]
cla2015 <-cla2015[which(cla2015$geog_l=="LA"),]
cla2016 <-cla2016[which(cla2016$geog_l=="LA"),]
cla2017 <-cla2017[which(cla2017$geog_l=="LA"),]

cl2011 <-cl2011[which(cl2011$geog_l=="LA"),]
cl2012 <-cl2012[which(cl2012$geog_l=="LA"),]
cl2013 <-cl2013[which(cl2013$geog_l=="LA"),]
cl2014 <-cl2014[which(cl2014$geog_l=="LA"),]
cl2015<-cl2015[which(cl2015$geog_l=="LA"),]
cl2016 <-cl2016[which(cl2016$geog_l=="LA"),]
cl2017 <-cl2017[which(cl2017$geog_l=="LA"),]

names(cl2014)[names(cl2014)=="All_aged1920.21"] <- "All_aged19.20.21"
names(cl2017)[names(cl2017)=="CL_All_19to21"] <- "All_aged19.20.21"

names(cla2011)[names(cla2011)=="CLA_Mar2011"] <- "CLA_Mar"
names(cla2012)[names(cla2012)=="CLA_Mar2012"] <- "CLA_Mar"
names(cla2013)[names(cla2013)=="CLA_Mar2013"] <- "CLA_Mar"
names(cla2014)[names(cla2014)=="CLA_Mar2014"] <- "CLA_Mar"
names(cla2015)[names(cla2015)=="CLA_Mar2015"] <- "CLA_Mar"
names(cla2016)[names(cla2016)=="CLA_Mar2016"] <- "CLA_Mar"
names(cla2017)[names(cla2017)=="CLA_Mar2017"] <- "CLA_Mar"

names(cla2011)[names(cla2011)=="CLA_stp2011"] <- "CLA_stp_during"
names(cla2012)[names(cla2012)=="CLA_stp2012"] <- "CLA_stp_during"
names(cla2013)[names(cla2013)=="CLA_stp2013"] <- "CLA_stp_during"
names(cla2014)[names(cla2014)=="CLA_stp2014"] <- "CLA_stp_during"
names(cla2015)[names(cla2015)=="CLA_stp2015"] <- "CLA_stp_during"
names(cla2016)[names(cla2016)=="CLA_stp2016"] <- "CLA_stp_during"
names(cla2017)[names(cla2017)=="CLA_stp2017"] <- "CLA_stp_during"

names(cla2011)[names(cla2011)=="CLA_2011"] <- "CLA_during"
names(cla2012)[names(cla2012)=="CLA_2012"] <- "CLA_during"
names(cla2013)[names(cla2013)=="CLA_2013"] <- "CLA_during"
names(cla2014)[names(cla2014)=="CLA_2014"] <- "CLA_during"
names(cla2015)[names(cla2015)=="CLA_2015"] <- "CLA_during"
names(cla2016)[names(cla2016)=="CLA_2016"] <- "CLA_during"
names(cla2017)[names(cla2017)=="CLA_2017"] <- "CLA_during"


names(cla2011)[names(cla2011)=="CLAA_InBound"] <- "CLA_InBound"
names(cla2012)[names(cla2012)=="CLAA_InBound"] <- "CLA_InBound"


#missing at 31 mar
#outsourcing
#demographics
#inside LA


cla2015$CLA_Miss <- NA
cla2016$CLA_Miss <- NA
cla2017$CLA_Miss <- NA

cla2015$CLA_Moth <- NA
cla2016$CLA_Moth <- NA
cla2017$CLA_Moth <- NA

cla2015$CLA_1Pla <- NA
cla2016$CLA_1Pla <- NA
cla2017$CLA_1Pla <- NA

cla2015$CLA_2PLa <- NA
cla2016$CLA_2PLa <- NA
cla2017$CLA_2PLa <- NA

cla2015$CLA_3Pla <- NA
cla2016$CLA_3Pla <- NA
cla2017$CLA_3Pla <- NA

cla2015$CLA_P2yrs <- NA
cla2016$CLA_P2yrs <- NA
cla2017$CLA_P2yrs <- NA

cla2016$CLA_OthPl <- NA

cla2017 <- cla2017[c(names(cla2011))]


CLA <- rbind(cla2011, cla2012, cla2013, cla2014, cla2015, cla2016, cla2017)
CLA <- merge(CLA, miss[-c(2,3,4)], by =c("New_geog_code", "year"), all=T)

setdiff(names(cla2011), names(cla2017))



cla2021 <- cla2021[which(cla2021$geographic_level=="Local authority"),]
cla2021stab <- cla2021stab[which(cla2021stab$geographic_level=="Local authority"),]
cla2021miss <- cla2021miss[which(cla2021miss$geographic_level=="Local authority"),]
cla2021short <- cla2021short[which(cla2021short$geographic_level=="Local authority"),]
cla2021convict <- cla2021convict[which(cla2021convict$geographic_level=="Local authority"),]
cla2021leaver <- cla2021leaver[which(cla2021leaver$geographic_level=="Local authority"),]
cla2021leaveracc <- cla2021leaveracc[which(cla2021leaveracc$geographic_level=="Local authority"),]
cla2021 <- cla2021[which(cla2021$characteristic!="Total"|(cla2021$characteristic=="Total"&cla2021$cla_group=="Category of need")),]
cla2021stab <- cla2021stab[which(cla2021stab$characteristic!="Total children"),]
cla2021leaver17 <- cla2021leaver[which(cla2021leaver$age=="Aged 17 to 18"),]
cla2021leaver19 <- cla2021leaver[which(cla2021leaver$age=="Aged 19 to 21"),]
cla2021leaveracc17 <- cla2021leaveracc[which(cla2021leaveracc$age=="17 to 18 years"),]
cla2021leaveracc19 <- cla2021leaveracc[which(cla2021leaveracc$age=="19 to 21 years"),]
cla2021convict <- cla2021convict[which(cla2021convict$cla_group=="Convictions: Children looked after Ages 10 to 17 years"),]


cla2021 <- cla2021[c(1,8,9,10,12,13)] %>% pivot_wider(names_from = characteristic, values_from = number, names_prefix = "CLA_")
cla2021stabper <- cla2021stab[c(1,8,9,10,12,14)] %>% pivot_wider(names_from = characteristic, values_from = percentage, names_prefix = "CLA_")
cla2021stab <- cla2021stab[c(1,8,9,10,12,13)] %>% pivot_wider(names_from = characteristic, values_from = number, names_prefix = "CLA_")
cla2021miss <- cla2021miss[c(1,10,11,12)] %>% pivot_wider(names_from = cla_group, values_from = number, names_prefix = "CLA_")
cla2021short <- cla2021short[c(1,10,11,12)] %>% pivot_wider(names_from = cla_group, values_from = number, names_prefix = "CLA_")
cla2021convict <- cla2021convict[c(1,10,12,13)] %>% pivot_wider(names_from = characteristic, values_from = number, names_prefix = "CLA_")
cla2021leaveracc19 <- cla2021leaveracc19[c(1,10,12,13)] %>% pivot_wider(names_from = accommodation_suitability, values_from = number, names_prefix = "CLA_19yr_")
cla2021leaveracc17 <- cla2021leaveracc17[c(1,10,12,13)] %>% pivot_wider(names_from = accommodation_suitability, values_from = number, names_prefix = "CLA_17yr_")
cla2021leaver17 <- cla2021leaver17[c(1,10,12,13)] %>% pivot_wider(names_from = in_touch, values_from = number, names_prefix = "CLA_17yr_")
cla2021leaver19 <- cla2021leaver19[c(1,10,12,13)] %>% pivot_wider(names_from = in_touch, values_from = number, names_prefix = "CLA_19yr_")


names(cla2021stabper)[names(cla2021stabper)=="�..time_period"] <- "year"
names(cla2021stabper)[names(cla2021stabper)=="old_la_code"] <- "geog_c"
names(cla2021stabper)[names(cla2021stabper)=="la_name"] <- "geog_n"
names(cla2021stabper)[names(cla2021stabper)=="new_la_code"] <- "New_geog_code"
names(cla2021stabper)[names(cla2021stabper)=="CLA_Living in the same placement for at least 2 years, or are placed for adoption and their adoption and their adoptive placement together with their previous placement, last for at least 2 years"] <- "CLA_P2yrs_PERCENT"



names(cla2021)[names(cla2021)=="time_period"] <- "year"
names(cla2021)[names(cla2021)=="old_la_code"] <- "geog_c"
names(cla2021)[names(cla2021)=="la_name"] <- "geog_n"
names(cla2021)[names(cla2021)=="new_la_code"] <- "New_geog_code"
names(cla2021)[names(cla2021)=="CLA_Total"] <- "CLA_Mar"
names(cla2021)[names(cla2021)=="CLA_1 to 4 years"] <- "CLA_1to4"
names(cla2021)[names(cla2021)=="CLA_5 to 9 years"] <- "CLA_5to9"
names(cla2021)[names(cla2021)=="CLA_15 to 15 years"] <- "CLA_10to15"
names(cla2021)[names(cla2021)=="CLA_Under 1 year"] <- "CLA_U1"
names(cla2021)[names(cla2021)=="CLA_16 years and over"] <- "CLA_16over"
names(cla2021)[names(cla2021)=="CLA_Asian or Asian British"] <- "CLA_Asian"
names(cla2021)[names(cla2021)=="CLA_Black, African, Caribbean or Black British"] <- "CLA_Black"
names(cla2021)[names(cla2021)=="CLA_Mixed or Multiple ethnic groups"] <- "CLA_Mixed"
names(cla2021)[names(cla2021)=="CLA_Other ethnic group"] <- "CLA_EOTH"
names(cla2021)[names(cla2021)=="CLA_Refused or information not yet available"] <- "CLA_OTH"
names(cla2021)[names(cla2021)=="CLA_Female"] <- "CLA_female"
names(cla2021)[names(cla2021)=="CLA_Male"] <- "CLA_male"
names(cla2021)[names(cla2021)=="CLA_Other placements"] <- "CLA_OthPl"
names(cla2021)[names(cla2021)=="CLA_Foster placements"] <- "CLA_Fost"
names(cla2021)[names(cla2021)=="CLA_Placed for adoption"] <- "CLA_Adopt"
names(cla2021)[names(cla2021)=="CLA_Parents or other person with parental responsibility"] <- "CLA_Parent"
names(cla2021)[names(cla2021)=="CLA_Other placements in the community"] <- "CLA_Ocom"
names(cla2021)[names(cla2021)=="CLA_Other residential settings"] <- "CLA_Ores"
names(cla2021)[names(cla2021)=="CLA_Residential schools"] <- "CLA_RSch"
names(cla2021)[names(cla2021)=="CLA_Secure units, children's homes and semi-independent living accommodation"] <- "CLA_Secure"
names(cla2021)[names(cla2021)=="CLA_unaccompanied asylum-seeking children"] <- "CLA_UASC"
names(cla2021)[names(cla2021)=="CLA_Private provision"] <- "CLA_Priv"
names(cla2021)[names(cla2021)=="CLA_Voluntary/third sector provision"] <- "CLA_Vol"
names(cla2021)[names(cla2021)=="CLA_Voluntary/third sector provision"] <- "CLA_Vol"


names(cla2021stab)[names(cla2021stab)=="CLA_Living in the same placement for at least 2 years, or are placed for adoption and their adoption and their adoptive placement together with their previous placement, last for at least 2 years"] <- "CLA_P2yrs"
names(cla2021stab)[names(cla2021stab)=="�..time_period"] <- "year"
names(cla2021stab)[names(cla2021stab)=="new_la_code"] <- "New_geog_code"
names(cla2021stab)[names(cla2021stab)=="old_la_code"] <- "geog_c"
names(cla2021stab)[names(cla2021stab)=="la_name"] <- "geog_n"

names(cla2021miss)[names(cla2021miss)=="new_la_code"] <- "New_geog_code"
names(cla2021miss)[names(cla2021miss)=="�..time_period"] <- "year"
names(cla2021miss)[names(cla2021miss)=="CLA_Children looked after during the year"] <- "CLA_during"
names(cla2021miss)[names(cla2021miss)=="CLA_Children who had a missing incident during the year"] <- "CLA_MissDuringYear"
names(cla2021miss)[names(cla2021miss)=="CLA_Number of missing incidents during the year"] <- "CLA_MissIncs"
names(cla2021miss)[names(cla2021miss)=="CLA_Children who went missing more than once during the year"] <- "CLA_MissMoreOnce"
names(cla2021miss)[names(cla2021miss)=="CLA_Children missing at 31 March"] <- "CLA_Miss31Mar"
names(cla2021miss)[names(cla2021miss)=="CLA_Children who were away from placement without authorisation during the year"] <- "CLA_AwayDuringYear"
names(cla2021miss)[names(cla2021miss)=="CLA_Number of away from placement without authorisation incidents during the year"] <- "CLA_AwayIncs"
names(cla2021miss)[names(cla2021miss)=="CLA_Children who were away from placement without authorisation more than once during the year"] <- "CLA_AwayMoreOnce"
names(cla2021miss)[names(cla2021miss)=="CLA_Children away from placement without authorisation at 31 March"] <- "CLA_Away31Mar"


names(cla2021short)[names(cla2021short)=="new_la_code"] <- "New_geog_code"
names(cla2021short)[names(cla2021short)=="time_period"] <- "year"
names(cla2021short)[names(cla2021short)=="CLA_Children who were only looked after exclusively under a series of short term placements"] <- "CLA_stp_during"

names(cla2021convict)[names(cla2021convict)=="new_la_code"] <- "New_geog_code"
names(cla2021convict)[names(cla2021convict)=="time_period"] <- "year"
names(cla2021convict)[names(cla2021convict)=="CLA_Convicted or subject to youth cautions, or youth conditional cautions during the year"] <- "CLA_convicted_during"
names(cla2021convict)[names(cla2021convict)=="CLA_Total ages 10 to 17 years"] <- "CLA_n_convictable_age_during"


names(cla2021leaver17)[names(cla2021leaver17)=="new_la_code"] <- "New_geog_code"
names(cla2021leaver17)[names(cla2021leaver17)=="time_period"] <- "year"
names(cla2021leaver17)[names(cla2021leaver17)=="CLA_17yr_Local authority in touch with care leaver"] <- "CLA_17yr_intouch"
names(cla2021leaver17)[names(cla2021leaver17)=="CLA_17yr_Local authority not in touch with care leaver"] <- "CLA_17yr_not_intouch"
names(cla2021leaver17)[names(cla2021leaver17)=="CLA_17yr_Total"] <- "CLA_17yr_total_n"
cla2021leaver17 <- cla2021leaver17[-c(6,7)]

names(cla2021leaver19)[names(cla2021leaver19)=="new_la_code"] <- "New_geog_code"
names(cla2021leaver19)[names(cla2021leaver19)=="time_period"] <- "year"
names(cla2021leaver19)[names(cla2021leaver19)=="CLA_19yr_Local authority in touch with care leaver"] <- "CLA_19yr_intouch"
names(cla2021leaver19)[names(cla2021leaver19)=="CLA_19yr_Local authority not in touch with care leaver"] <- "CLA_19yr_not_intouch"
names(cla2021leaver19)[names(cla2021leaver19)=="CLA_19yr_Total"] <- "CLA_19yr_total_n"
cla2021leaver19 <- cla2021leaver19[-c(6,7)]


names(cla2021leaveracc17)[names(cla2021leaveracc17)=="new_la_code"] <- "New_geog_code"
names(cla2021leaveracc17)[names(cla2021leaveracc17)=="time_period"] <- "year"
names(cla2021leaveracc17)[names(cla2021leaveracc17)=="CLA_17yr_Accommodation considered not suitable"] <- "CLA_17yr_acc_not_suit"
names(cla2021leaveracc17)[names(cla2021leaveracc17)=="CLA_17yr_Accommodation considered suitable"] <- "CLA_17yr_acc_suit"
cla2021leaveracc17 <- cla2021leaveracc17[-c(5,6)]

names(cla2021leaveracc19)[names(cla2021leaveracc19)=="new_la_code"] <- "New_geog_code"
names(cla2021leaveracc19)[names(cla2021leaveracc19)=="time_period"] <- "year"
names(cla2021leaveracc19)[names(cla2021leaveracc19)=="CLA_19yr_Accommodation considered not suitable"] <- "CLA_19yr_acc_not_suit"
names(cla2021leaveracc19)[names(cla2021leaveracc19)=="CLA_19yr_Accommodation considered suitable"] <- "CLA_19yr_acc_suit"
cla2021leaveracc19 <- cla2021leaveracc19[-c(5,6)]

cla2021 <- merge(cla2021, cla2021stab, by=c("year", "New_geog_code", "geog_c", "geog_n"),all=T)
cla2021 <- merge(cla2021, cla2021miss, by=c("year", "New_geog_code"), all=T)
cla2021 <- merge(cla2021, cla2021short, by=c("year", "New_geog_code"), all=T)




names(cla2021)[names(cla2021)=="CLA_Placed inside the local authority boundary"] <- "CLA_InBound"
names(cla2021)[names(cla2021)=="time_period"] <- "year"

rm(cla2011)
rm(cla2012)
rm(cla2013)
rm(cla2014)
rm(cla2015)
rm(cla2016)
rm(cla2017)









df <-  rbind(cla2021[c("New_geog_code","geog_n","year" ,"CLA_Priv", "CLA_InBound","CLA_Mar", "CLA_Vol", "CLA_P2yrs", "CLA_Fost", "CLA_White", "CLA_female", "CLA_during", "CLA_stp_during","CLA_Away31Mar", "CLA_AwayMoreOnce", "CLA_AwayIncs", "CLA_AwayDuringYear", "CLA_Miss31Mar", "CLA_MissMoreOnce", "CLA_MissIncs", "CLA_MissDuringYear")], CLA[c("New_geog_code","geog_n","year" ,"CLA_Priv","CLA_InBound" ,"CLA_Mar", "CLA_Vol", "CLA_P2yrs", "CLA_Fost", "CLA_White","CLA_female", "CLA_during","CLA_stp_during" ,"CLA_Away31Mar", "CLA_AwayMoreOnce", "CLA_AwayIncs", "CLA_AwayDuringYear", "CLA_Miss31Mar", "CLA_MissMoreOnce", "CLA_MissIncs", "CLA_MissDuringYear")])

df <- merge(df, cla2021stabper[c("New_geog_code","year","CLA_P2yrs_PERCENT")], by=c("New_geog_code","year"),all=T)




df$per_for_profit <- as.double(df$CLA_Priv)/as.double(df$CLA_Mar)*100
df$per_third_sector <- as.double(df$CLA_Vol)/as.double(df$CLA_Mar)*100
df$per_white_percent <- as.double(df$CLA_White)/as.double(df$CLA_Mar)*100
df$per_female_percent <- as.double(df$CLA_female)/as.double(df$CLA_Mar)*100
df$per_foster_percent <- as.double(df$CLA_Fost)/as.double(df$CLA_Mar)*100
df$CLA_Mar <- as.double(df$CLA_Mar)
df$per_short_term <- as.double(df$CLA_stp_during)/as.double(df$CLA_during)*100
df$per_less_than_2yrs <- 100-(as.double(df$CLA_P2yrs)/as.double(df$CLA_Mar)*100)
df$per_outside_LA <- 100-(as.double(df$CLA_InBound)/as.double(df$CLA_Mar)*100)
df$per_missing_incidents <- as.double(df$CLA_MissDuringYear)/as.double(df$CLA_during)*100
df$per_away_incidents <- as.double(df$CLA_AwayDuringYear)/as.double(df$CLA_during)*100
df$per_acc_not_suit <- as.double(df)/as.double(df$CLA_during)*100




region <- read.csv("Data/LA_Care_Expenditure_By_Ownership_OverTime.csv")

region <- region[c(6,10)]
region <- unique(region)
region <- region[which(region$LAD19CD!=""),]
names(region)[names(region)=="LAD19CD"] <- "New_geog_code"

df <- merge(df, region, by="New_geog_code", all.x=T)
df[df$New_geog_code=="E08000020",]$region_name <- "North East"
df[df$New_geog_code=="E06000060",]$region_name <- "South East"
df[df$New_geog_code=="E06000048",]$region_name <- "North East"


df <- df[complete.cases(df$region_name),]

alternative <- cla2021[c("New_geog_code", "year", "CLA_Placed more than 20 miles from home", "CLA_With 3 or more placements during the year")]


#post-rejection
# df <- merge(df, cla2021convict, by=c("year", "New_geog_code"), all=T)
# df <- merge(df, cla2021leaver17, by=c("year", "New_geog_code"), all=T)
# df <- merge(df, cla2021leaver19, by=c("year", "New_geog_code"), all=T)
# df <- merge(df, cla2021leaveracc17, by=c("year", "New_geog_code"), all=T)
# df <- merge(df, cla2021leaveracc19, by=c("year", "New_geog_code"), all=T)

df <- merge(df, alternative,by=c("New_geog_code", "year"), all.x=T)
rm(list=setdiff(ls(), "df"))



#Expenditure data

outturn1011 <- read.csv("Data/outturn_1011.csv")
outturn1112 <- read.csv("Data/outturn_1112.csv")
outturn1213 <- read.csv("Data/outturn_1213.csv")
outturn1314 <- read.csv("Data/outturn_1314.csv")


ExpenditureData <- read.csv("Data/LA_Care_Expenditure_By_Ownership_OverTime.csv")
ExpenditureData <- ExpenditureData[which(ExpenditureData$geographic_level =="Local authority"),]

ExpenditureData$year <- str_sub(ExpenditureData$�..time_period, start= -2)
ExpenditureData$year <-  paste("20", ExpenditureData$year, sep="")
ExpenditureData <- ExpenditureData[which(ExpenditureData$Description=="3.1.11 Total Children Looked After"),]
ExpenditureData <- ExpenditureData[c("year", "LAD19CD", "OwnProvision","PrivateProvision","OtherPublic","Voluntary","TotalExpenditure" )]


names(ExpenditureData)[names(ExpenditureData)=="PrivateProvision"] <- "Private_spend"
names(ExpenditureData)[names(ExpenditureData)=="OwnProvision"] <- "LA_own_spend"
names(ExpenditureData)[names(ExpenditureData)=="OtherPublic"] <- "Other_Public_spend"
names(ExpenditureData)[names(ExpenditureData)=="Voluntary"] <- "Voluntary_spend"
names(ExpenditureData)[names(ExpenditureData)=="TotalExpenditure"] <- "Total_spend"

expen21 <- read.csv("Data/CLA_expenditure2021.csv")
expen21$year <- 2021
expen21 <- expen21[c("year", "location_code", "own_provision","private_provision","other_public_sector_provision","voluntary_provision","total_expenditure" )]

names(expen21)[names(expen21)=="private_provision"] <- "Private_spend"
names(expen21)[names(expen21)=="own_provision"] <- "LA_own_spend"
names(expen21)[names(expen21)=="other_public_sector_provision"] <- "Other_Public_spend"
names(expen21)[names(expen21)=="voluntary_provision"] <- "Voluntary_spend"
names(expen21)[names(expen21)=="total_expenditure"] <- "Total_spend"
names(expen21)[names(expen21)=="location_code"] <- "LAD19CD"


#keep just residential care spend
outturn1011CLA <- outturn1011[which(outturn1011$S52.Line.Reference =="Total Children Looked After"),]
outturn1112CLA <- outturn1112[which(outturn1112$LineNumber =="12"),]
outturn1213CLA <- outturn1213[which(outturn1213$LineNumber =="15"),]
outturn1314CLA <- outturn1314[which(outturn1314$LineNumber =="16"),]

#Standardise variable names
#in thousands of pounds fyi



names(outturn1011CLA)[names(outturn1011CLA)=="Private..z.i.."] <- "Private_spend"
names(outturn1011CLA)[names(outturn1011CLA)=="Voluntary..z.iii.."] <- "Voluntary_spend"
names(outturn1011CLA)[names(outturn1011CLA)=="Own.Provision..y."] <- "LA_own_spend"
names(outturn1011CLA)[names(outturn1011CLA)=="Total.Expenditure..k."] <- "Total_spend"
names(outturn1011CLA)[names(outturn1011CLA)=="Other.Public..z.ii.."] <- "Other_Public_spend"


names(outturn1112CLA)[names(outturn1112CLA)=="PrivateProvision"] <- "Private_spend"
names(outturn1112CLA)[names(outturn1112CLA)=="Voluntary"] <- "Voluntary_spend"
names(outturn1112CLA)[names(outturn1112CLA)=="OwnProvision"] <- "LA_own_spend"
names(outturn1112CLA)[names(outturn1112CLA)=="TotalExpenditure"] <- "Total_spend"
names(outturn1112CLA)[names(outturn1112CLA)=="OtherPublic"] <- "Other_Public_spend"

names(outturn1213CLA)[names(outturn1213CLA)=="PrivateProvision"] <- "Private_spend"
names(outturn1213CLA)[names(outturn1213CLA)=="Voluntary"] <- "Voluntary_spend"
names(outturn1213CLA)[names(outturn1213CLA)=="OwnProvision"] <- "LA_own_spend"
names(outturn1213CLA)[names(outturn1213CLA)=="TotalExpenditure"] <- "Total_spend"
names(outturn1213CLA)[names(outturn1213CLA)=="OtherPublic"] <- "Other_Public_spend"

names(outturn1314CLA)[names(outturn1314CLA)=="PrivateProvision"] <- "Private_spend"
names(outturn1314CLA)[names(outturn1314CLA)=="Voluntary"] <- "Voluntary_spend"
names(outturn1314CLA)[names(outturn1314CLA)=="OwnProvision"] <- "LA_own_spend"
names(outturn1314CLA)[names(outturn1314CLA)=="TotalExpenditure"] <- "Total_spend"
names(outturn1314CLA)[names(outturn1314CLA)=="OtherPublic"] <- "Other_Public_spend"

#keep important vars


outturn1011CLA <- outturn1011CLA[c(1,6,7,8,9, 10)]
outturn1112CLA <- outturn1112CLA[c(1,5,6,7,8,9)]
outturn1213CLA <- outturn1213CLA[c(1,6,7,8,9, 10)]
outturn1314CLA <- outturn1314CLA[c(1,6,7,8,9, 10)]

outturn1011CLA$year <- 2011
outturn1112CLA$year <- 2012
outturn1213CLA$year <- 2013
outturn1314CLA$year <- 2014

oldlalookup <- read.csv("Data/oldlalookup.csv")



outturnla <- rbind(outturn1011CLA,outturn1112CLA,
                   outturn1213CLA,outturn1314CLA)

outturnla <- merge(oldlalookup, outturnla, by="LA", all=T)
outturnla <- outturnla[-c(1)]



outturn <- rbind(ExpenditureData, expen21)
outturn$LA_own_spend <- as.double(outturn$LA_own_spend)*1000000
outturn$Private_spend <- as.double(outturn$Private_spend)*1000000
outturn$Other_Public_spend <- as.double(outturn$Other_Public_spend)*1000000
outturn$Voluntary_spend <- as.double(outturn$Voluntary_spend)*1000000
outturn$Total_spend <- as.double(outturn$Total_spend)*1000000
outturn <- rbind(outturn, outturnla)

names(outturn)[names(outturn)=="LAD19CD"] <- "New_geog_code"


df <- merge(df, outturn, by=c("year", "New_geog_code"), all.x=T)


df$CLA_Mar <- as.double(df$CLA_Mar)
df$Total_spend <- as.double(df$Total_spend)/1000000
df$CLA_MissDuringYear <- as.double(df$CLA_MissIncs)

df$three_or_more_percent <- as.double(df$`CLA_With 3 or more placements during the year`)/as.double(df$CLA_during)*100
df$more_20 <- as.double(df$`CLA_Placed more than 20 miles from home`)/as.double(df$CLA_Mar)*100

df$in_touch_17yrs_percent <- as.double(df$CLA_17yr_intouch)/as.double(df$CLA_17yr_total_n)*100
df$in_touch_19yrs_percent <- as.double(df$CLA_19yr_intouch)/as.double(df$CLA_19yr_total_n)*100
df$acc_suit_17yrs_percent <- as.double(df$CLA_17yr_acc_suit)/as.double(df$CLA_17yr_total_n)*100
df$acc_suit_19yrs_percent <- as.double(df$CLA_19yr_acc_suit)/as.double(df$CLA_19yr_total_n)*100

df$convicted_percent <- as.double(df$CLA_convicted_during)/as.double(df$CLA_n_convictable_age_during)*100



lanames <- read.csv("Data/2022_characteristics.csv")

lanames <- lanames[c("la_name", "new_la_code")]

lanames <- unique(lanames)

names(lanames)[names(lanames)=="la_name"] <- "LAD19NM"
names(lanames)[names(lanames)=="new_la_code"] <- "New_geog_code"

lanames$LAD19NM[lanames$LAD19NM=="Bristol, City of"] <- "Bristol"
lanames$LAD19NM[lanames$LAD19NM=="Herefordshire, County of"] <- "Herefordshire"
lanames$LAD19NM[lanames$LAD19NM=="Kingston upon Hull, City of"] <- "Kingston upon Hull"
lanames$LAD19NM[lanames$LAD19NM=="Richmond upon Thames"] <- "Richmond Upon Thames"
lanames$LAD19NM[lanames$LAD19NM=="St. Helens"] <- "St Helens"
lanames$LAD19NM[lanames$LAD19NM=="Southend-on-Sea"] <- "Southend on Sea"



LAData2021 <- read.csv("Data/la_new_of.csv")
LAData2021$date <- as.Date(LAData2021$Inspection.date, format =  "%d/%m/%Y")
LAData2021$year <- format(LAData2021$date,"%Y")

# LAData2021[which(LAData2021$year=="2016"),]$year <- 2018
# LAData2021[which(LAData2021$year=="2017"),]$year <- 2018

names(LAData2021)[names(LAData2021)=="Overall.effectiveness"] <- "over"
names(LAData2021)[names(LAData2021)=="Impact.of.leaders"] <- "lead"
names(LAData2021)[names(LAData2021)=="Experiences.and.progress.of.children.who.need.help.and.protection"] <- "safe"
names(LAData2021)[names(LAData2021)=="Experiences.and.progress.of.children.in.care.and.care.leavers"] <- "exper"

LAData2021 <- merge(lanames, LAData2021[c("LAD19NM", "year", "over", "lead", "safe", "exper")], by=c("LAD19NM"), all=T )

LAData2021 <- LAData2021[complete.cases(LAData2021$over),]
df <- merge(df, LAData2021, by=c("New_geog_code", "year"),all.x=T) 

rm(list=setdiff(ls(), "df"))

# ####adding in total children during year####
# 
# df <- df[complete.cases(df$geog_n),]
# 
# cla2021convict <- read.csv("Data/la_conviction_health_outcome_cla.csv")
# 
# cla2021convict <- cla2021convict[which(cla2021convict$geographic_level=="Local authority"),]
# 
# cla2021convict <- cla2021convict[which(cla2021convict$characteristic!="Total all ages"|cla2021convict$cla_group=="Substance misuse: Children looked after all ages"),]
# 
# #cla2021convict <- cla2021convict[which(cla2021convict$cla_group=="Ages 5 to 16 years with SDQ score"),]
# #cla2021convictper <- cla2021convict[c(1,9,10,12,13)] %>% pivot_wider(names_from = characteristic, values_from = percent, names_prefix = "CLA_")
# cla2021convict <- cla2021convict[c(1,9,10,12,13)] %>% pivot_wider(names_from = characteristic, values_from = number, names_prefix = "CLA_")
# names(cla2021convict)[names(cla2021convict)=="la_name"] <- "geog_n"
# names(cla2021convict)[names(cla2021convict)=="time_period"] <- "year"
# 
# # cla2021convict <- cla2021convict[which(cla2021convict$cla_group=="Ages 5 to 16 years with SDQ score"),]
# # #cla2021convictper <- cla2021convict[c(1,9,10,12,13)] %>% pivot_wider(names_from = characteristic, values_from = percent, names_prefix = "CLA_")
# # cla2021convict <- cla2021convict[c(1,9,10,12,13)] %>% pivot_wider(names_from = characteristic, values_from = number, names_prefix = "CLA_")
# # names(cla2021convict)[names(cla2021convict)=="la_name"] <- "geog_n"
# # names(cla2021convict)[names(cla2021convict)=="time_period"] <- "year"
# 
# 
# sdq12 <- read.csv("Data/sdq12.csv")
# sdq14 <- read.csv("Data/sdq14.csv")
# 
# ud10 <- read.csv("Data/health_UD_2010.csv")
# ud11 <- read.csv("Data/health_UD_2011.csv")
# ud12 <- read.csv("Data/health_UD_2012.csv")
# 
# checks13 <- read.csv("Data/healthchecks_2013.csv")
# checks14 <- read.csv("Data/healthchecks_2014.csv")
# 
# substance13 <- read.csv("Data/substance_2013.csv")
# substance14 <- read.csv("Data/substance_2014.csv")
# 
# convict13 <- read.csv("Data/convict_2013.csv")
# convict14 <- read.csv("Data/convict_2014.csv")
# 
# 
# sdq15 <- read.csv("Data/sdq15.csv")
# sdq16 <- read.csv("Data/sdq16.csv")
# sdq17 <- read.csv("Data/sdq17.csv")
# 
# sdq10 <- sdq12[c(1,2,3,4,5,6,7,8)]
# sdq11 <- sdq12[c(1, 10,11,12,13,14,15,16)]
# sdq12 <- sdq12[c(1,18,19,20,21,22,23,24)]
# 
# sdq13 <- sdq14[c(1,2,3,4,5,6,7,8)]
# sdq14 <- sdq14[c(1,10,11,12,13,14,15,16)]
# 
# names(ud10)[names(ud10)=="X"] <-"geog_n"
# names(ud11)[names(ud11)=="X"] <-"geog_n"
# names(ud12)[names(ud12)=="la_name"] <-"geog_n"
# 
# 
# ud10$geog_n <-  gsub('&','and',ud10$geog_n)
# sdq10$geog_n <-  gsub('&','and',sdq10$geog_n)
# ud11$geog_n <-  gsub('&','and',ud11$geog_n)
# sdq11$geog_n <-  gsub('&','and',sdq11$geog_n)
# ud12$geog_n <-  gsub('&','and',ud12$geog_n)
# sdq12$geog_n <-  gsub('&','and',sdq12$geog_n)
# df$geog_n <-  gsub('&','and',df$geog_n)
# 
# ud10$geog_n <-  toupper(ud10$geog_n)
# ud11$geog_n <-  toupper(ud11$geog_n)
# sdq10$geog_n <-  toupper(sdq10$geog_n)
# sdq11$geog_n <-  toupper(sdq11$geog_n)
# sdq12$geog_n <-  toupper(sdq12$geog_n)
# ud12$geog_n <-  toupper(ud12$geog_n)
# df$geog_n <-  toupper(df$geog_n)
# 
# ud10$geog_n[ud10$geog_n!="CITY OF LONDON"] <-  gsub('CITY','',ud10$geog_n[ud10$geog_n!="CITY OF LONDON"])
# sdq10$geog_n[sdq10$geog_n!="CITY OF LONDON"] <-  gsub('CITY','',sdq10$geog_n[sdq10$geog_n!="CITY OF LONDON"])
# ud11$geog_n[ud11$geog_n!="CITY OF LONDON"] <-  gsub('CITY','',ud11$geog_n[ud11$geog_n!="CITY OF LONDON"])
# sdq11$geog_n[sdq11$geog_n!="CITY OF LONDON"] <-  gsub('CITY','',sdq11$geog_n[sdq11$geog_n!="CITY OF LONDON"])
# ud12$geog_n[ud12$geog_n!="CITY OF LONDON"] <-  gsub('CITY','',ud12$geog_n[ud12$geog_n!="CITY OF LONDON"])
# sdq12$geog_n[sdq12$geog_n!="CITY OF LONDON"] <-  gsub('CITY','',sdq12$geog_n[sdq12$geog_n!="CITY OF LONDON"])
# df$geog_n[df$geog_n!="CITY OF LONDON"] <-  gsub('CITY','',df$geog_n[df$geog_n!="CITY OF LONDON"])
# 
# ud10$geog_n[ud10$geog_n!="CITY OF LONDON"] <-  gsub('OF','',ud10$geog_n[ud10$geog_n!="CITY OF LONDON"])
# sdq10$geog_n[sdq10$geog_n!="CITY OF LONDON"] <-  gsub('OF','',sdq10$geog_n[sdq10$geog_n!="CITY OF LONDON"])
# ud11$geog_n[ud11$geog_n!="CITY OF LONDON"] <-  gsub('OF','',ud11$geog_n[ud11$geog_n!="CITY OF LONDON"])
# sdq11$geog_n[sdq11$geog_n!="CITY OF LONDON"] <-  gsub('OF','',sdq11$geog_n[sdq11$geog_n!="CITY OF LONDON"])
# ud12$geog_n[ud12$geog_n!="CITY OF LONDON"] <-  gsub('OF','',ud12$geog_n[ud12$geog_n!="CITY OF LONDON"])
# sdq12$geog_n[sdq12$geog_n!="CITY OF LONDON"] <-  gsub('OF','',sdq12$geog_n[sdq12$geog_n!="CITY OF LONDON"])
# df$geog_n[df$geog_n!="CITY OF LONDON"] <-  gsub('OF','',df$geog_n[df$geog_n!="CITY OF LONDON"])
# 
# 
# ud10$geog_n <-  gsub('ROYAL BOROUGH','',ud10$geog_n)
# sdq10$geog_n <-  gsub('ROYAL BOROUGH','',sdq10$geog_n)
# ud11$geog_n <-  gsub('ROYAL BOROUGH','',ud11$geog_n)
# sdq11$geog_n <-  gsub('ROYAL BOROUGH','',sdq11$geog_n)
# ud12$geog_n <-  gsub('ROYAL BOROUGH','',ud12$geog_n)
# sdq12$geog_n <-  gsub('ROYAL BOROUGH','',sdq12$geog_n)
# df$geog_n <-  gsub('ROYAL BOROUGH','',df$geog_n)
# 
# ud10$geog_n <-  gsub('[[:punct:] ]+',' ',ud10$geog_n)
# sdq10$geog_n <-  gsub('[[:punct:] ]+',' ',sdq10$geog_n)
# ud11$geog_n <-  gsub('[[:punct:] ]+',' ',ud11$geog_n)
# sdq11$geog_n <-  gsub('[[:punct:] ]+',' ',sdq11$geog_n)
# ud12$geog_n <-  gsub('[[:punct:] ]+',' ',ud12$geog_n)
# sdq12$geog_n <-  gsub('[[:punct:] ]+',' ',sdq12$geog_n)
# df$geog_n <-  gsub('[[:punct:] ]+',' ',df$geog_n)
# 
# 
# ud10$geog_n <-  str_trim(ud10$geog_n)
# ud11$geog_n <-  str_trim(ud11$geog_n)
# sdq10$geog_n <-  str_trim(sdq10$geog_n)
# sdq11$geog_n <-  str_trim(sdq11$geog_n)
# sdq12$geog_n <-  str_trim(sdq12$geog_n)
# ud12$geog_n <-  str_trim(ud12$geog_n)
# df$geog_n <-  str_trim(df$geog_n)
# 
# ud10$geog_n[ud10$geog_n=="BLACKBURN AND DARWIN"] <- "BLACKBURN WITH DARWEN"
# ud10$geog_n[ud10$geog_n=="STOKE"] <- "STOKE ON TRENT"
# ud10$geog_n[ud10$geog_n=="SOUTHEND"] <- "SOUTHEND ON SEA"
# ud10$geog_n[ud10$geog_n=="BATH AND NE SOMERSET"] <- "BATH AND NORTH EAST SOMERSET"
# 
# ud11$geog_n[ud11$geog_n=="BEDFORD BOROUGH"] <- "BEDFORD"
# 
# ud12$geog_n[ud12$geog_n=="BLACKBURN AND DARWIN"] <- "BLACKBURN WITH DARWEN"
# ud12$geog_n[ud12$geog_n=="STOKE"] <- "STOKE ON TRENT"
# ud12$geog_n[ud12$geog_n=="SOUTHEND"] <- "SOUTHEND ON SEA"
# ud12$geog_n[ud12$geog_n=="BATH AND NE SOMERSET"] <- "BATH AND NORTH EAST SOMERSET"
# 
# 
# 
# sdq10 <- merge(sdq10[which(sdq10$geog_n!=""),], ud10[which(ud10$geog_n!=""),], by="geog_n", all=T)
# sdq11 <- merge(sdq11[which(sdq11$geog_n!=""),], ud11[which(ud11$geog_n!=""),], by="geog_n", all=T)
# sdq12 <- merge(sdq12[which(sdq12$geog_n!=""),], ud12[which(ud12$geog_n!=""),], by="geog_n", all=T)
# 
# names(sdq13)[names(sdq13)=="�..geog_n"] <-"geog_n"
# names(sdq14)[names(sdq14)=="�..geog_n"] <-"geog_n"
# names(checks13)[names(checks13)=="�.."] <-"geog_n"
# names(substance13)[names(substance13)=="�.."] <-"geog_n"
# names(convict13)[names(convict13)=="�.."] <-"geog_n"
# names(checks14)[names(checks14)=="�.."] <-"geog_n"
# names(substance14)[names(substance14)=="�.."] <-"geog_n"
# names(convict14)[names(convict14)=="�.."] <-"geog_n"
# 
# sdq14$geog_n[sdq14$geog_n=="Merton2"] <- "Merton"
# checks14$geog_n[checks14$geog_n=="Merton2"] <- "Merton"
# 
# 
# sdq13 <- merge(sdq13[which(sdq13$geog_n!=""),], checks13[which(checks13$geog_n!=""),], by="geog_n", all=T)
# sdq13 <- merge(sdq13, substance13[which(substance13$geog_n!=""),], by="geog_n", all=T)
# sdq13 <- merge(sdq13, convict13[which(convict13$geog_n!=""),], by="geog_n", all=T)
# 
# sdq14 <- merge(sdq14[which(sdq14$geog_n!=""),], checks14[which(checks14$geog_n!=""),], by="geog_n", all=T)
# sdq14 <- merge(sdq14, substance14[which(substance14$geog_n!=""),], by="geog_n", all=T)
# sdq14 <- merge(sdq14, convict14[which(convict14$geog_n!=""),], by="geog_n", all=T)
# 
# 
# 
# sdq10$year <- 2010
# sdq11$year <- 2011
# sdq12$year <- 2012
# sdq13$year <- 2013
# sdq14$year <- 2014
# sdq15$year <- 2015
# sdq16$year <- 2016
# sdq17$year <- 2017
# 
# 
# 
# 
# names(sdq10)[names(sdq10)=="Number.of.children.looked.after.continuously.for.12.months.at.31.March.2010"] <- "CLA_Total all ages"
# names(sdq11)[names(sdq11)=="Number.of.children.looked.after.continuously.for.12.months.at.31.March.2011"] <- "CLA_Total all ages"
# names(sdq12)[names(sdq12)=="CLA_12_months_2012"] <- "CLA_Total all ages"
# names(sdq13)[names(sdq13)=="Number.of.children.looked.after.at.31.March.who.had.been.looked.after.for.at.least.12.months1.x"] <- "CLA_Total all ages"
# names(sdq14)[names(sdq14)=="Number.of.children.looked.after.at.31.March.who.had.been.looked.after.for.at.least.12.months1.x"] <- "CLA_Total all ages"
# names(sdq15)[names(sdq15)=="CLA_12mths"] <- "CLA_Total all ages"
# names(sdq16)[names(sdq16)=="CLA_12mths"] <- "CLA_Total all ages"
# names(sdq17)[names(sdq17)=="OC2_12mths"] <- "CLA_Total all ages"
# 
# 
# sdq <- rbind(sdq10[c("geog_n", "year", "CLA_Total all ages")],
#              sdq11[c("geog_n", "year", "CLA_Total all ages")],
#              sdq12[c("geog_n", "year", "CLA_Total all ages")],
#              sdq13[c("geog_n", "year", "CLA_Total all ages")],
#              sdq14[c("geog_n", "year", "CLA_Total all ages")],
#              sdq15[c("geog_n", "year", "CLA_Total all ages")],
#              sdq16[c("geog_n", "year", "CLA_Total all ages")],
#              sdq17[c("geog_n", "year", "CLA_Total all ages")],
#              cla2021convict[c("geog_n", "year", "CLA_Total all ages")])
# 
# df$geog_n <-  gsub('&','and',df$geog_n)
# sdq$geog_n <-  gsub('&','and',sdq$geog_n)
# 
# df$geog_n <-  toupper(df$geog_n)
# sdq$geog_n <-  toupper(sdq$geog_n)
# 
# df$geog_n[df$geog_n!="CITY OF LONDON"] <-  gsub('CITY','',df$geog_n[df$geog_n!="CITY OF LONDON"])
# sdq$geog_n[sdq$geog_n!="CITY OF LONDON"] <-  gsub('CITY','',sdq$geog_n[sdq$geog_n!="CITY OF LONDON"])
# 
# df$geog_n[df$geog_n!="CITY OF LONDON"] <-  gsub('OF','',df$geog_n[df$geog_n!="CITY OF LONDON"])
# sdq$geog_n[sdq$geog_n!="CITY OF LONDON"] <-  gsub('OF','',sdq$geog_n[sdq$geog_n!="CITY OF LONDON"])
# 
# 
# df$geog_n <-  gsub('ROYAL BOROUGH','',df$geog_n)
# sdq$geog_n <-  gsub('ROYAL BOROUGH','',sdq$geog_n)
# 
# df$geog_n <-  gsub('[[:punct:] ]+',' ',df$geog_n)
# sdq$geog_n <-  gsub('[[:punct:] ]+',' ',sdq$geog_n)
# 
# df$geog_n <-  str_trim(df$geog_n)
# sdq$geog_n <-  str_trim(sdq$geog_n)
# 
# 
# 
# 
# df <- merge(df, sdq, by=c("geog_n", "year"), all=T)



df <- df[complete.cases(df$CLA_Priv),]

df$CLA_P2yrs_PERCENTminus <- 100-as.numeric(df$CLA_P2yrs_PERCENT)

#save data
write.csv(df,"Data/placement_data_final.csv")
#ffs <- df %>% dplyr::group_by(New_geog_code, year) %>%mutate(nobs=n())%>%dplyr::filter(nobs>1)

####ANALYSIS####
####Pretty Graphs####

