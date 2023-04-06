Create_figure_1 <- function(){
  
  df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/CSC_Outsourcing_Placement_Outcomes/main/Data/placement_data_final.csv"))
  df$year <- factor(df$year)
  stab <- ggplot(df, aes(x = per_less_than_2yrs, y = year, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_distiller(palette="Spectral")+
    labs(title = 'Placement Instability\n - relative to all children in care', x="Placements less than 2 years (%)", y="Year") +
    theme_nice() +
    xlim(0,100)+
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 10),
      text = element_text(size=15))
  
  stab2 <- ggplot(df, aes(x = CLA_P2yrs_PERCENTminus, y = year, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_distiller(palette="Spectral")+
    labs(title = 'Placement Instability\n - relative to children who have been looked after for 2.5 years', x="Placements less than 2 years (%)", y="Year") +
    theme_nice() +
    xlim(0,100)+
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 10),
      text = element_text(size=15))
  
  
  out <- ggplot(df, aes(x = per_for_profit, y = year, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_distiller(palette="Spectral")+
    labs(title = 'For-Profit Outsourcing', x="For-profit provision (%)", y="Year") +
    theme_nice() +
    xlim(0,100)+
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8),
      text = element_text(size=15))
  
  laout <- ggplot(df, aes(x = per_outside_LA, y = year, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_distiller(palette="Spectral")+
    labs(title = 'Placements at Distance', x="Placements outside home LA boundary (%)", y="Year") +
    theme_nice() +
    xlim(0,100)+
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8),
      text = element_text(size=15))
  
  misspl <- ggplot(df, aes(x = per_missing_incidents, y = year, fill = ..x..)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_distiller(palette="Spectral")+
    labs(title = 'Missing Incidents', x="Missing Incidents during year (% of children in care)", y="Year") +
    theme_nice() +
    xlim(0,100)+
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8),
      text = element_text(size=15))
  
  
  

  
  
  dfaverage <- aggregate(. ~New_geog_code, data=df[c("New_geog_code", "per_less_than_2yrs", "per_for_profit","per_outside_LA", "per_missing_incidents", "CLA_P2yrs_PERCENTminus")], mean)
  
  laboundaries <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2019_GCB_UK_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
  uaboundaries <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Counties_and_Unitary_Authorities_December_2019_FCB_UK_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
 # laboundaries <- readShapeSpatial("Data/laboundaries")
#  uaboundaries <- readShapeSpatial("Data/uaboundaries")
  names(laboundaries)[names(laboundaries)=="lad19cd"] <- "New_geog_code"
  names(uaboundaries)[names(uaboundaries)=="ctyua19cd"] <- "New_geog_code"
  names(uaboundaries)[names(uaboundaries)=="ctyua19nm"] <- "lad19nm"
  names(uaboundaries)[names(uaboundaries)=="ctyua19nmw"] <- "lad19nmw"
  
  uaboundaries <- uaboundaries[which(uaboundaries$lad19nm!="Wales"),]
  uaboundaries <- uaboundaries[which(uaboundaries$lad19nm!="Scotland"),]
  
  uaboundaries <- uaboundaries[grepl('^E', uaboundaries$New_geog_code),] 

  map <-uaboundaries
  
  
  map <- merge(map, dfaverage, by=c("New_geog_code"),all=T)
  map <- st_as_sf(map)
  
  no_classes <- 6
  
  
  quantiles <- quantile(as.double(map$per_less_than_2yrs), 
                        probs = seq(0, 1, length.out = no_classes + 1), na.rm=T)
  
  # here I define custom labels (the default ones would be ugly)
  labels <- c()
  for(idx in 1:length(quantiles)){
    labels <- c(labels, paste0(round(quantiles[idx], 2), 
                               " - ", 
                               round(quantiles[idx + 1], 2)))
  }
  # I need to remove the last label 
  # because that would be something like "66.62 - NA"
  labels <- labels[1:length(labels)-1]
  
  # here I actually create a new 
  # variable on the dataset with the quantiles
  map$stable_quantiles <- cut(as.double(map$per_less_than_2yrs), 
                              breaks = quantiles, 
                              labels = labels, 
                              include.lowest = T)
  
  theme_map <- function(...) {
    #theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position="bottom",
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_rect(fill = "white", color = NA), # bg of the panel
      plot.background = element_rect(fill = "white", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      # legend.background = element_rect(fill = "transparent", color=NA), # get rid of legend bg
      #  legend.box.background = element_rect(fill = "transparent", color=NA),
      # panel.border = element_blank(),legend.title=element_text(size=8), 
      #  legend.text=element_text(size=7),legend.key.size = unit(0.3, "cm"),
      ...
    )
  }
  
  
  
  library(RColorBrewer)
  
  
  
  unstable_map <- ggplot(data = map) +
    geom_sf(aes(fill = stable_quantiles), color = NA) +
    theme_map()+
    labs(x = NULL, 
         y = NULL)+
    scale_fill_brewer(
      palette = "OrRd",
      name = "Placements < 2 years\n(Average %, 2011-2022)", na.value="grey")+
    theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))
  
  
  
  
  quantiles <- quantile(as.double(map$CLA_P2yrs_PERCENTminus), 
                        probs = seq(0, 1, length.out = no_classes + 1), na.rm=T)
  
  # here I define custom labels (the default ones would be ugly)
  labels <- c()
  for(idx in 1:length(quantiles)){
    labels <- c(labels, paste0(round(quantiles[idx], 2), 
                               " - ", 
                               round(quantiles[idx + 1], 2)))
  }
  # I need to remove the last label 
  # because that would be something like "66.62 - NA"
  labels <- labels[1:length(labels)-1]
  
  # here I actually create a new 
  # variable on the dataset with the quantiles
  map$unstable2_quantiles <- cut(as.double(map$CLA_P2yrs_PERCENTminus), 
                                 breaks = quantiles, 
                                 labels = labels, 
                                 include.lowest = T)
  
  
  unstable2_map <- ggplot(data = map) +
    geom_sf(aes(fill = unstable2_quantiles), color = NA) +
    theme_map()+
    labs(x = NULL, 
         y = NULL)+
    scale_fill_brewer(
      palette = "OrRd",
      name = "Placements < 2 years\n(Average %, 2011-2022)", na.value="grey")+
    theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))
  
  
  
  quantiles <- quantile(as.double(map$per_outside_LA), 
                        probs = seq(0, 1, length.out = no_classes + 1), na.rm=T)
  
  # here I define custom labels (the default ones would be ugly)
  labels <- c()
  for(idx in 1:length(quantiles)){
    labels <- c(labels, paste0(round(quantiles[idx], 2), 
                               " - ", 
                               round(quantiles[idx + 1], 2)))
  }
  # I need to remove the last label 
  # because that would be something like "66.62 - NA"
  labels <- labels[1:length(labels)-1]
  
  # here I actually create a new 
  # variable on the dataset with the quantiles
  map$laout_quantiles <- cut(as.double(map$per_outside_LA), 
                             breaks = quantiles, 
                             labels = labels, 
                             include.lowest = T)
  
  
  distance_map <- ggplot(data = map) +
    geom_sf(aes(fill = laout_quantiles), color = NA) +
    theme_map()+
    labs(x = NULL, 
         y = NULL)+
    scale_fill_brewer(
      palette = "OrRd",
      name = "Placements outside LA\n(Average %, 2011-2022)", na.value="grey")+
    theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))
  
  
  
  quantiles <- quantile(as.double(map$per_for_profit), 
                        probs = seq(0, 1, length.out = no_classes + 1), na.rm=T)
  
  # here I define custom labels (the default ones would be ugly)
  labels <- c()
  for(idx in 1:length(quantiles)){
    labels <- c(labels, paste0(round(quantiles[idx], 2), 
                               " - ", 
                               round(quantiles[idx + 1], 2)))
  }
  # I need to remove the last label 
  # because that would be something like "66.62 - NA"
  labels <- labels[1:length(labels)-1]
  
  # here I actually create a new 
  # variable on the dataset with the quantiles
  map$for_profit_percent_quantiles <- cut(as.double(map$per_for_profit), 
                                          breaks = quantiles, 
                                          labels = labels, 
                                          include.lowest = T)
  
  
  for_profit_percent_map <- ggplot(data = map) +
    geom_sf(aes(fill = for_profit_percent_quantiles), color = NA) +
    theme_map()+
    labs(x = NULL, 
         y = NULL)+
    scale_fill_brewer(
      palette = "OrRd",
      name = "For-profit provision\n(Average %, 2011-2022)", na.value="grey")+
    theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))
  
  
  quantiles <- quantile(as.double(map$per_missing_incidents), 
                        probs = seq(0, 1, length.out = no_classes + 1), na.rm=T)
  
  # here I define custom labels (the default ones would be ugly)
  labels <- c()
  for(idx in 1:length(quantiles)){
    labels <- c(labels, paste0(round(quantiles[idx], 2), 
                               " - ", 
                               round(quantiles[idx + 1], 2)))
  }
  # I need to remove the last label 
  # because that would be something like "66.62 - NA"
  labels <- labels[1:length(labels)-1]
  
  # here I actually create a new 
  # variable on the dataset with the quantiles
  map$per_missing_incidents_quantiles <- cut(as.double(map$per_missing_incidents), 
                                             breaks = quantiles, 
                                             labels = labels, 
                                             include.lowest = T)
  
  
  per_missing_incidents_quantiles_map <- ggplot(data = map) +
    geom_sf(aes(fill = per_missing_incidents_quantiles), color = NA) +
    theme_map()+
    labs(x = NULL, 
         y = NULL)+
    scale_fill_brewer(
      palette = "OrRd",
      name = "Missing Incidents\n(Average, % of CIC, 2011-2022)", na.value="grey")+
    theme(text=element_text(size=10), legend.key.size = unit(0.65, "cm"), legend.title = element_text(size=9))
  
  
  

  regions <- unique(df[c("New_geog_code", "region_name")])
  

  df <- df%>% mutate(region_no = ifelse(region_name=="East Midlands", 1, ifelse(region_name=="East Midlands", 1,
                                                                                ifelse(region_name=="East of England", 2,
                                                                                       ifelse(region_name=="East Midlands", 3,
                                                                                              ifelse(region_name=="Inner London", 4,
                                                                                                     ifelse(region_name=="Outer London", 5,
                                                                                                            ifelse(region_name=="North East", 6,
                                                                                                                   ifelse(region_name=="North West", 7,
                                                                                                                          ifelse(region_name=="South East", 8,
                                                                                                                                 ifelse(region_name=="South West", 9,
                                                                                                                                        ifelse(region_name=="West Midlands", 10,
                                                                                                                                               ifelse(region_name=="Yorkshire and the Humber", 11,NA )))))))))))))
  plotdata <- df[complete.cases(df$region_name),]

  anotherunstableplot <- ggplot(plotdata, aes(x=year, y= reorder(New_geog_code, -region_no) ,fill=per_less_than_2yrs))+
    geom_tile(color="transparent" )+theme_minimal()+
    scale_fill_distiller(palette="Spectral",na.value="white")+
    scale_y_discrete(breaks=c("E10000007","E06000056","E09000022","E06000057", "E08000007", "E09000016", "E06000045", "E06000029", "E08000027", "E08000018"),
                     labels=c("East Midlands", "East of England", "Inner London", "North East", "North West", "Outer London", "South East", "South West", "West Midlands", "Yorkshire & Humber"))+
    labs(x="Year", y="Regions", fill = "Placements < 2-years\n(%)")+
    theme_nice()+
    geom_hline(yintercept = c(15.5,29.5,47.5, 66.5, 89.5, 101.5, 120.5, 134.5, 145.5), colour="black", linetype = "dotted")+
    theme(text=element_text(size=10.9) ,legend.key.height = unit(2, 'cm'),legend.key.size = unit(0.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.25, hjust=0.5))
  
  anotherunstable2plot <- ggplot(plotdata, aes(x=year, y= reorder(New_geog_code, -region_no) ,fill=CLA_P2yrs_PERCENTminus))+
    geom_tile(color="transparent" )+theme_minimal()+
    scale_fill_distiller(palette="Spectral",na.value="white")+
    scale_y_discrete(breaks=c("E10000007","E06000056","E09000022","E06000057", "E08000007", "E09000016", "E06000045", "E06000029", "E08000027", "E08000018"),
                     labels=c("East Midlands", "East of England", "Inner London", "North East", "North West", "Outer London", "South East", "South West", "West Midlands", "Yorkshire & Humber"))+
    labs(x="Year", y="Regions", fill = "Placements < 2-years\n(%)")+
    theme_nice()+
    geom_hline(yintercept = c(15.5,29.5,47.5, 66.5, 89.5, 101.5, 120.5, 134.5, 145.5), colour="black", linetype = "dotted")+
    theme(text=element_text(size=10.9) ,legend.key.height = unit(2, 'cm'),legend.key.size = unit(0.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.25, hjust=0.5))
  
  
  anotherdistanceplot <- ggplot(plotdata, aes(x=year, y= reorder(New_geog_code, -region_no) ,fill=per_outside_LA))+
    geom_tile(color="transparent" )+theme_minimal()+
    scale_fill_distiller(palette="Spectral", na.value="white")+
    scale_y_discrete(breaks=c("E10000007","E06000056","E09000022","E06000057", "E08000007", "E09000016", "E06000045", "E06000029", "E08000027", "E08000018"),
                     labels=c("East Midlands", "East of England", "Inner London", "North East", "North West", "Outer London", "South East", "South West", "West Midlands", "Yorkshire & Humber"))+
    labs(x="Year", y="Regions", fill = "Placements outside LA\n(%)")+
    theme_nice()+
    geom_hline(yintercept = c(15.5,29.5,47.5, 66.5, 89.5, 101.5, 120.5, 134.5, 145.5), colour="black", linetype = "dotted")+
    theme(text=element_text(size=10.9) ,legend.key.height = unit(2, 'cm'),legend.key.size = unit(0.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.25, hjust=0.5))
  
  anotherprofitplot <- ggplot(plotdata, aes(x=year, y= reorder(New_geog_code, -region_no) ,fill=per_for_profit))+
    geom_tile(color="transparent" )+theme_minimal()+
    scale_fill_distiller(palette="Spectral", na.value="white")+
    scale_y_discrete(breaks=c("E10000007","E06000056","E09000022","E06000057", "E08000007", "E09000016", "E06000045", "E06000029", "E08000027", "E08000018"),
                     labels=c("East Midlands", "East of England", "Inner London", "North East", "North West", "Outer London", "South East", "South West", "West Midlands", "Yorkshire & Humber"))+
    labs(x="Year", y="Regions", fill = "For-profit provision\n(%)")+
    theme_nice()+
    geom_hline(yintercept = c(15.5,29.5,47.5, 66.5, 89.5, 101.5, 120.5, 134.5, 145.5), colour="black", linetype = "dotted")+
    theme(text=element_text(size=10.9) ,legend.key.height = unit(2, 'cm'),legend.key.size = unit(0.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.25, hjust=0.5))
  
  anothermissingplot <- ggplot(plotdata, aes(x=year, y= reorder(New_geog_code, -region_no) ,fill=per_missing_incidents))+
    geom_tile(color="transparent" )+theme_minimal()+
    scale_fill_distiller(palette="Spectral", na.value="white")+
    scale_y_discrete(breaks=c("E10000007","E06000056","E09000022","E06000057", "E08000007", "E09000016", "E06000045", "E06000029", "E08000027", "E08000018"),
                     labels=c("East Midlands", "East of England", "Inner London", "North East", "North West", "Outer London", "South East", "South West", "West Midlands", "Yorkshire & Humber"))+
    labs(x="Year", y="Regions", fill = "Missing Incidents\n(% of CIC)")+
    theme_nice()+
    geom_hline(yintercept = c(15.5,29.5,47.5, 66.5, 89.5, 101.5, 120.5, 134.5, 145.5), colour="black", linetype = "dotted")+
    theme(text=element_text(size=10.9) ,legend.key.height = unit(2, 'cm'),legend.key.size = unit(0.5, "cm"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.25, hjust=0.5))
  
  

  
  
  fig11_lines_across <- cowplot::plot_grid(
    # col 1
    cowplot::plot_grid(laout, distance_map, anotherdistanceplot, nrow = 1,rel_widths = c(1, 0.8, 1), labels = c('A', 'B', 'C')) +
      theme(plot.background = element_rect(color = "black", fill="white"),
            panel.border = element_rect(colour = "black", fill=NA, size=1)
      ),
    
    # col 2
    cowplot::plot_grid(stab,unstable_map ,anotherunstableplot, nrow= 1, rel_widths = c(1, 0.8, 1),labels = c('D', 'E', 'F')) +
      theme(plot.background = element_rect(color = "black", fill="white"),
            panel.border = element_rect(colour = "black", fill=NA, size=1)
      ), 
    
    
    #  col 3
    cowplot::plot_grid(stab2,unstable2_map ,anotherunstable2plot, nrow= 1,rel_widths = c(1, 0.8, 1), labels = c('G', 'H', 'I')) +
      theme(plot.background = element_rect(color = "black", fill="white"),
            panel.border = element_rect(colour = "black", fill=NA, size=1)
      ), 
    # 
    # col 3
    cowplot::plot_grid(out,for_profit_percent_map ,anotherprofitplot, nrow= 1, rel_widths = c(1, 0.8, 1),labels = c('J', 'K', 'L')) +
      theme(plot.background = element_rect(color = "black", fill="white"),
            panel.border = element_rect(colour = "black", fill=NA, size=1)
      ), 
    
    ncol = 1)
  
}