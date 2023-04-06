Create_figure_3 <- function(){
  df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/CSC_Outsourcing_Placement_Outcomes/main/Data/placement_data_final.csv"))
  df$year <- factor(df$year)
   fig2 <- df
  
  #calculate raw number of out of bound children
  
  fig2$outbound <- as.double(fig2$CLA_Mar)-as.double(fig2$CLA_InBound)
  
  #Remove missing data for out of bound and private placement values
  
  fig2 <- fig2[complete.cases(fig2$outbound),]
  fig2 <- fig2[complete.cases(fig2$per_for_profit),]
  
  #See which LAs we have full 12 year observations for
  
  fig2 <- fig2 %>% dplyr::group_by(New_geog_code) %>% mutate(nobs = n()) %>% ungroup()
  
  #Keep only LAs we have 12 years of data for
  
  fig2 <- fig2[which(fig2$nobs==12),]
  
  
  fig2$CLA_Priv <-as.double(fig2$CLA_Priv)
  
  #Run our FE model
  
  finalFEusingLM <- lm(outbound~CLA_Priv+per_foster_percent+per_white_percent+per_female_percent+CLA_Mar+Total_spend+per_short_term+New_geog_code+year,  data=fig2)
  
  #Calculate our AME and CIs
  
  multiplier <- finalFEusingLM$coefficients["CLA_Priv"]
  multiplierlower <- deframe(conf_int(finalFEusingLM,  vcov = "CR2", cluster = fig2$New_geog_code, test = "Satterthwaite",coefs="CLA_Priv")[5])
  multiplierupper <- deframe(conf_int(finalFEusingLM, vcov = "CR2", cluster = fig2$New_geog_code, test = "Satterthwaite" ,coefs="CLA_Priv")[6])
  
  #Create lagged variables to have columns with changes in any year
  
  lagged <- fig2[c("CLA_Priv","outbound", "New_geog_code", "year")]
  lagged$year <- as.character(as.double(as.character(lagged$year))+1)
  
  names(lagged)[names(lagged)=="CLA_Priv"] <- "lagged_profit"
  names(lagged)[names(lagged)=="outbound"] <- "lagged_outbound"
  
  fig2 <- merge(fig2, lagged, by=c("New_geog_code", "year"),all.x=T)
  
  #Create changes in key variables
  
  fig2$profitchange <- fig2$CLA_Priv-fig2$lagged_profit
  fig2$outboundchange <- fig2$outbound-fig2$lagged_outbound
  
  
  fig2 <- fig2[complete.cases(fig2$New_geog_code),]
  
  #Calculate the additional children out of place we expect for every LA in every year given their changes in private placements
  
  fig2$addoutbounds <- ((fig2$profitchange))*multiplier
  fig2$addoutboundslwr <- ((fig2$profitchange))*multiplierlower
  fig2$addoutboundsupr <- ((fig2$profitchange))*multiplierupper
  
  #create plotting variable
  
  fig2$group=1
  
  #make first year the same as observed value
  
  fig2$syntheticvalue <- fig2$outbound 
  fig2$syntheticupper <- fig2$outbound 
  fig2$syntheticlower <- fig2$outbound 
  
  fig2[which(fig2$year==2011),]$syntheticvalue <- fig2[which(fig2$year==2011),]$outbound
  fig2[which(fig2$year==2011),]$syntheticupper <- fig2[which(fig2$year==2011),]$outbound
  fig2[which(fig2$year==2011),]$syntheticlower <- fig2[which(fig2$year==2011),]$outbound
  
  #For second year, calculate the change by minussing the additional out of bound children from the observed change. Add this change to previous years value
  #Synthetic change = (fig2[which(fig2$year==2012),]$outboundchange)-(fig2[which(fig2$year==2012),]$addoutbounds)
  
  
  fig2[which(fig2$year==2012),]$syntheticvalue <- fig2[which(fig2$year==2011),]$syntheticvalue+(fig2[which(fig2$year==2012),]$outboundchange)-(fig2[which(fig2$year==2012),]$addoutbounds)
  fig2[which(fig2$year==2012),]$syntheticupper <- fig2[which(fig2$year==2011),]$syntheticupper+(fig2[which(fig2$year==2012),]$outboundchange)-(fig2[which(fig2$year==2012),]$addoutboundsupr)
  fig2[which(fig2$year==2012),]$syntheticlower <- fig2[which(fig2$year==2011),]$syntheticlower+(fig2[which(fig2$year==2012),]$outboundchange)-(fig2[which(fig2$year==2012),]$addoutboundslwr)
  
  #Repeat steps
  
  fig2[which(fig2$year==2013),]$syntheticvalue <- fig2[which(fig2$year==2012),]$syntheticvalue+(fig2[which(fig2$year==2013),]$outboundchange)-(fig2[which(fig2$year==2013),]$addoutbounds)
  fig2[which(fig2$year==2013),]$syntheticupper <- fig2[which(fig2$year==2012),]$syntheticupper+(fig2[which(fig2$year==2013),]$outboundchange)-(fig2[which(fig2$year==2013),]$addoutboundsupr)
  fig2[which(fig2$year==2013),]$syntheticlower <- fig2[which(fig2$year==2012),]$syntheticlower+(fig2[which(fig2$year==2013),]$outboundchange)-(fig2[which(fig2$year==2013),]$addoutboundslwr)
  
  fig2[which(fig2$year==2014),]$syntheticvalue <- fig2[which(fig2$year==2013),]$syntheticvalue+(fig2[which(fig2$year==2014),]$outboundchange)-(fig2[which(fig2$year==2014),]$addoutbounds)
  fig2[which(fig2$year==2014),]$syntheticupper <- fig2[which(fig2$year==2013),]$syntheticupper+(fig2[which(fig2$year==2014),]$outboundchange)-(fig2[which(fig2$year==2014),]$addoutboundsupr)
  fig2[which(fig2$year==2014),]$syntheticlower <- fig2[which(fig2$year==2013),]$syntheticlower+(fig2[which(fig2$year==2014),]$outboundchange)-(fig2[which(fig2$year==2014),]$addoutboundslwr)
  
  
  fig2[which(fig2$year==2015),]$syntheticvalue <- fig2[which(fig2$year==2014),]$syntheticvalue+(fig2[which(fig2$year==2015),]$outboundchange)-(fig2[which(fig2$year==2015),]$addoutbounds)
  fig2[which(fig2$year==2015),]$syntheticupper <- fig2[which(fig2$year==2014),]$syntheticupper+(fig2[which(fig2$year==2015),]$outboundchange)-(fig2[which(fig2$year==2015),]$addoutboundsupr)
  fig2[which(fig2$year==2015),]$syntheticlower <- fig2[which(fig2$year==2014),]$syntheticlower+(fig2[which(fig2$year==2015),]$outboundchange)-(fig2[which(fig2$year==2015),]$addoutboundslwr)
  
  fig2[which(fig2$year==2016),]$syntheticvalue <- fig2[which(fig2$year==2015),]$syntheticvalue+(fig2[which(fig2$year==2016),]$outboundchange)-(fig2[which(fig2$year==2016),]$addoutbounds)
  fig2[which(fig2$year==2016),]$syntheticupper <- fig2[which(fig2$year==2015),]$syntheticupper+(fig2[which(fig2$year==2016),]$outboundchange)-(fig2[which(fig2$year==2016),]$addoutboundsupr)
  fig2[which(fig2$year==2016),]$syntheticlower <- fig2[which(fig2$year==2015),]$syntheticlower+(fig2[which(fig2$year==2016),]$outboundchange)-(fig2[which(fig2$year==2016),]$addoutboundslwr)
  
  
  fig2[which(fig2$year==2017),]$syntheticvalue <- fig2[which(fig2$year==2016),]$syntheticvalue+(fig2[which(fig2$year==2017),]$outboundchange)-(fig2[which(fig2$year==2017),]$addoutbounds)
  fig2[which(fig2$year==2017),]$syntheticupper<- fig2[which(fig2$year==2016),]$syntheticupper+(fig2[which(fig2$year==2017),]$outboundchange)-(fig2[which(fig2$year==2017),]$addoutboundsupr)
  fig2[which(fig2$year==2017),]$syntheticlower <- fig2[which(fig2$year==2016),]$syntheticlower+(fig2[which(fig2$year==2017),]$outboundchange)-(fig2[which(fig2$year==2017),]$addoutboundslwr)
  
  fig2[which(fig2$year==2018),]$syntheticvalue <- fig2[which(fig2$year==2017),]$syntheticvalue+(fig2[which(fig2$year==2018),]$outboundchange)-(fig2[which(fig2$year==2018),]$addoutbounds)
  fig2[which(fig2$year==2018),]$syntheticupper<- fig2[which(fig2$year==2017),]$syntheticupper+(fig2[which(fig2$year==2018),]$outboundchange)-(fig2[which(fig2$year==2018),]$addoutboundsupr)
  fig2[which(fig2$year==2018),]$syntheticlower <- fig2[which(fig2$year==2017),]$syntheticlower+(fig2[which(fig2$year==2018),]$outboundchange)-(fig2[which(fig2$year==2018),]$addoutboundslwr)
  
  fig2[which(fig2$year==2019),]$syntheticvalue <- fig2[which(fig2$year==2018),]$syntheticvalue+(fig2[which(fig2$year==2019),]$outboundchange)-(fig2[which(fig2$year==2019),]$addoutbounds)
  fig2[which(fig2$year==2019),]$syntheticupper<- fig2[which(fig2$year==2018),]$syntheticupper+(fig2[which(fig2$year==2019),]$outboundchange)-(fig2[which(fig2$year==2019),]$addoutboundsupr)
  fig2[which(fig2$year==2019),]$syntheticlower <- fig2[which(fig2$year==2018),]$syntheticlower+(fig2[which(fig2$year==2019),]$outboundchange)-(fig2[which(fig2$year==2019),]$addoutboundslwr)
  
  fig2[which(fig2$year==2020),]$syntheticvalue <- fig2[which(fig2$year==2019),]$syntheticvalue+(fig2[which(fig2$year==2020),]$outboundchange)-(fig2[which(fig2$year==2020),]$addoutbounds)
  fig2[which(fig2$year==2020),]$syntheticupper<- fig2[which(fig2$year==2019),]$syntheticupper+(fig2[which(fig2$year==2020),]$outboundchange)-(fig2[which(fig2$year==2020),]$addoutboundsupr)
  fig2[which(fig2$year==2020),]$syntheticlower <- fig2[which(fig2$year==2019),]$syntheticlower+(fig2[which(fig2$year==2020),]$outboundchange)-(fig2[which(fig2$year==2020),]$addoutboundslwr)
  
  fig2[which(fig2$year==2021),]$syntheticvalue <- fig2[which(fig2$year==2020),]$syntheticvalue+(fig2[which(fig2$year==2021),]$outboundchange)-(fig2[which(fig2$year==2021),]$addoutbounds)
  fig2[which(fig2$year==2021),]$syntheticupper<- fig2[which(fig2$year==2020),]$syntheticupper+(fig2[which(fig2$year==2021),]$outboundchange)-(fig2[which(fig2$year==2021),]$addoutboundsupr)
  fig2[which(fig2$year==2021),]$syntheticlower <- fig2[which(fig2$year==2020),]$syntheticlower+(fig2[which(fig2$year==2021),]$outboundchange)-(fig2[which(fig2$year==2021),]$addoutboundslwr)
  
  fig2[which(fig2$year==2022),]$syntheticvalue <- fig2[which(fig2$year==2021),]$syntheticvalue+(fig2[which(fig2$year==2022),]$outboundchange)-(fig2[which(fig2$year==2022),]$addoutbounds)
  fig2[which(fig2$year==2022),]$syntheticupper<- fig2[which(fig2$year==2021),]$syntheticupper+(fig2[which(fig2$year==2022),]$outboundchange)-(fig2[which(fig2$year==2022),]$addoutboundsupr)
  fig2[which(fig2$year==2022),]$syntheticlower <- fig2[which(fig2$year==2021),]$syntheticlower+(fig2[which(fig2$year==2022),]$outboundchange)-(fig2[which(fig2$year==2022),]$addoutboundslwr)
  
  #Calculate our totals - the difference in the sums of our observed out of bounds every year and synthetic values
  
  tot <- sum(fig2[complete.cases(fig2$syntheticvalue),]$outbound, na.rm=T)-sum(fig2$syntheticvalue, na.rm=T)
  lwr <- sum(fig2[complete.cases(fig2$syntheticvalue),]$outbound, na.rm=T)-sum(fig2$syntheticupper, na.rm=T)
  upr <- sum(fig2[complete.cases(fig2$syntheticvalue),]$outbound, na.rm=T)-sum(fig2$syntheticlower, na.rm=T)
  
  #create simple df for plotting annually
  
  fig2yr <-   fig2[c("year","syntheticvalue","syntheticupper" ,"syntheticlower","outbound")]
  fig2yr <- aggregate(. ~year, data=fig2yr, sum,  na.rm=TRUE, na.action=NULL)
  
  fig2yr$group=1
  
  
  fig2yrs$group <- 1
  
  #plot
  
  plot2 <- 
    ggplot(fig2yr, aes(x = year, y = outbound, group=group)) +
    geom_line()+
    geom_line(aes(y=syntheticvalue),linetype = "dashed", color = "darkgray")+
    geom_ribbon(aes(ymin = syntheticlower, ymax = syntheticupper), 
                alpha=0.1, 
                linetype="dashed",
                color="lightgray")+
    labs(x="Year", y="Outside of LA placements (n)")+
    theme_nice()
  
  plot2
  
}