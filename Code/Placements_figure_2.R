Create_figure_2 <- function(){
  df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/CSC_Outsourcing_Placement_Outcomes/main/Data/placement_data_final.csv"))
  df$year <- factor(df$year)
  
  dfchange <- df
  
  dfchange11 <- dfchange[which(dfchange$year=="2011"),]
  dfchange22 <- dfchange[which(dfchange$year=="2022"),]
  
  dfchange11 <- dfchange11[c("New_geog_code", "per_for_profit", "per_outside_LA", "per_less_than_2yrs")]
  dfchange22 <- dfchange22[c("New_geog_code", "per_for_profit", "per_outside_LA", "per_less_than_2yrs", "CLA_Mar","region_name")]
  
  names(dfchange11)[names(dfchange11)=="per_for_profit"] <- "profit_11"
  names(dfchange11)[names(dfchange11)=="per_outside_LA"] <- "outsidela_11"
  names(dfchange11)[names(dfchange11)=="per_less_than_2yrs"] <- "unstable_11"
  
  names(dfchange22)[names(dfchange22)=="per_for_profit"] <- "profit_22"
  names(dfchange22)[names(dfchange22)=="per_outside_LA"] <- "outsidela_22"
  names(dfchange22)[names(dfchange22)=="per_less_than_2yrs"] <- "unstable_22"
  
  
  
  dfchange <- merge(dfchange11, dfchange22, by="New_geog_code", all=T)
  
  
  dfchange$profitchange <- dfchange$profit_22-dfchange$profit_11
  dfchange$unstablechange <- dfchange$unstable_22-dfchange$unstable_11
  dfchange$outsidelachange <- dfchange$outsidela_22-dfchange$outsidela_11
  
  dfchange <- dfchange[complete.cases(dfchange),]
  
  x <- ggplot(dfchange,aes(x=profitchange, y=outsidelachange, size=CLA_Mar))+
    geom_point(aes(color=region_name))+
    labs(color="Region",size="Children in Care (n)")
  
  x <- cowplot::get_legend(x)
  
  
  a <- ggplot(dfchange, aes(x=profitchange, y=outsidelachange, size=CLA_Mar))+
    geom_point(aes(color=region_name, alpha=0.3))+
    theme_nice()+
    labs(title = "Placements at Distance" ,x="", y="Change in placements outside LA boundary\n(% points, 2011-22)")+
    stat_smooth(method="lm")+
    geom_vline(xintercept = 0, linetype="dashed")+
    geom_hline(yintercept = 0, linetype="dashed")+
    theme(axis.text.x =element_blank(), legend.position = "none")
  
  b <- ggplot(dfchange, aes(x=profitchange, y=unstablechange, size=CLA_Mar))+
    geom_point(aes(color=region_name, alpha=0.3))+
    theme_nice()+
    labs(title="Placement Instability (Duration)",x="Change in for-profit outsourcing (% points, 2011-22)", y="Change in placements outside LA boundary\n(% points, 2011-22)")+
    stat_smooth(method="lm")+
    geom_vline(xintercept = 0, linetype="dashed")+
    geom_hline(yintercept = 0, linetype="dashed")+
    theme(legend.position = "none")
  
  fig1 <- cowplot::plot_grid(a,b,ncol=1)
  fig1 <- cowplot::plot_grid(fig1, x, ncol=2, rel_widths = c(0.8,0.2))
  
fig1
  
}