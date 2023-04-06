Create_table_1 <- function(){
  
  df <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/CSC_Outsourcing_Placement_Outcomes/main/Data/placement_data_final.csv"))
  df$year <- factor(df$year)
  
  CBPS_data <- df[complete.cases(df$per_for_profit),]
  CBPS_data <- CBPS_data[complete.cases(CBPS_data$Total_spend),]
  CBPS_data <- CBPS_data[complete.cases(CBPS_data$per_foster_percent),]
  CBPS_data <- CBPS_data[complete.cases(CBPS_data$per_female_percent),]
  CBPS_data <- CBPS_data[complete.cases(CBPS_data$per_white_percent),]
  CBPS_data <- CBPS_data[complete.cases(CBPS_data$per_short_term),]
  CBPS_data <- CBPS_data[complete.cases(CBPS_data$CLA_Mar),]
  CBPS_data <- CBPS_data[complete.cases(CBPS_data$New_geog_code),]
  
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  fit <- quiet(npCBPS(per_for_profit ~ per_short_term+Total_spend, data = CBPS_data))
  
  
  
  
  FEloc <-plm(per_outside_LA~per_for_profit, data=df, index=c("New_geog_code", "year") ,method="within", effect = "twoways")
  FEloccon <-plm(per_outside_LA~per_for_profit+per_foster_percent+per_white_percent+per_female_percent+CLA_Mar+Total_spend+per_short_term, data=df, index=c("New_geog_code", "year") ,method="within", effect = "twoways")
  FEloccon_cbps <-lm(per_outside_LA~per_for_profit+per_foster_percent+per_white_percent+per_female_percent+CLA_Mar+Total_spend+per_short_term+New_geog_code+year, data=CBPS_data, weights = fit$weights)
  FEstabe <-plm(per_less_than_2yrs~per_for_profit, data=df, index=c("New_geog_code", "year") ,method="within", effect = "twoways")
  FEstabecon <-plm(per_less_than_2yrs~per_for_profit+per_foster_percent+per_white_percent+per_female_percent+CLA_Mar+Total_spend+per_short_term, data=df, index=c("New_geog_code", "year") ,method="within", effect = "twoways")
  FEstabecon_cbps <-lm(per_less_than_2yrs~per_for_profit+per_foster_percent+per_white_percent+per_female_percent+CLA_Mar+Total_spend+per_short_term+New_geog_code+year, data=CBPS_data, weights = fit$weights)
  #FEmiss <-plm(per_missing_incidents~per_for_profit, data=df, index=c("New_geog_code", "year") ,method="within", effect = "twoways")
  #FEmisscon <-plm(per_missing_incidents~per_for_profit+per_foster_percent+per_white_percent+per_female_percent+CLA_Mar+Total_spend+per_short_term, data=df, index=c("New_geog_code", "year") ,method="within", effect = "twoways")
  
  
  
  FElocsum <- as.list(modelsummary(FEloc, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
  FElocconsum <- as.list(modelsummary(FEloccon, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
  FElocconsum_cbps <- as.list(modelsummary(FEloccon_cbps, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
  FEstabesum <- as.list(modelsummary(FEstabe, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
  FEstabeconsum <- as.list(modelsummary(FEstabecon, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
  FEstabeconsum_cbps <- as.list(modelsummary(FEstabecon_cbps, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
  
  #FEmisssum <- as.list(modelsummary(FEmiss, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
  #FEmissconsum <- as.list(modelsummary(FEmisscon, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
  
  
  FElocsum$tidy$p.value <- coef_test(FEloc, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$p
  FElocsum$tidy$std.error <- coef_test(FEloc, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE
  FElocsum$tidy$conf.low <- FElocsum$tidy$estimate-(1.96*coef_test(FEloc, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE)
  FElocsum$tidy$conf.high <- FElocsum$tidy$estimate+(1.96*coef_test(FEloc, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE)
  FElocsum$tidy$estimate <- FElocsum$tidy$estimate
  
  FElocconsum$tidy$p.value <- coef_test(FEloccon, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$p
  FElocconsum$tidy$std.error <- coef_test(FEloccon, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE
  FElocconsum$tidy$conf.low <- FElocconsum$tidy$estimate-(1.96*coef_test(FEloccon, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE)
  FElocconsum$tidy$conf.high <- FElocconsum$tidy$estimate+(1.96*coef_test(FEloccon, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE)
  FElocconsum$tidy$estimate <- FElocconsum$tidy$estimate
  
  FElocconsum_cbps$tidy$p.value <- coef_test(FEloccon_cbps, vcov = "CR2", cluster = CBPS_data$New_geog_code, test = "Satterthwaite")$p
  FElocconsum_cbps$tidy$std.error <- coef_test(FEloccon_cbps, vcov = "CR2", cluster = CBPS_data$New_geog_code, test = "Satterthwaite")$SE
  FElocconsum_cbps$tidy$conf.low <- FElocconsum_cbps$tidy$estimate-(1.96*coef_test(FEloccon_cbps, vcov = "CR2", cluster = CBPS_data$New_geog_code, test = "Satterthwaite")$SE)
  FElocconsum_cbps$tidy$conf.high <- FElocconsum_cbps$tidy$estimate+(1.96*coef_test(FEloccon_cbps, vcov = "CR2", cluster = CBPS_data$New_geog_code, test = "Satterthwaite")$SE)
  FElocconsum_cbps$tidy$estimate <- FElocconsum_cbps$tidy$estimate
  
  FEstabesum$tidy$p.value <- coef_test(FEstabe, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$p
  FEstabesum$tidy$std.error <- coef_test(FEstabe, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE
  FEstabesum$tidy$conf.low <- FEstabesum$tidy$estimate-(1.96*coef_test(FEstabe, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE)
  FEstabesum$tidy$conf.high <- FEstabesum$tidy$estimate+(1.96*coef_test(FEstabe, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE)
  FEstabesum$tidy$estimate <- FEstabesum$tidy$estimate
  
  FEstabeconsum$tidy$p.value <- coef_test(FEstabecon, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$p
  FEstabeconsum$tidy$std.error <- coef_test(FEstabecon, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE
  FEstabeconsum$tidy$conf.low <- FEstabeconsum$tidy$estimate-(1.96*coef_test(FEstabecon, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE)
  FEstabeconsum$tidy$conf.high <- FEstabeconsum$tidy$estimate+(1.96*coef_test(FEstabecon, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE)
  FEstabeconsum$tidy$estimate <- FEstabeconsum$tidy$estimate
  
  
  FEstabeconsum_cbps$tidy$p.value <- coef_test(FEstabecon_cbps, vcov = "CR2", cluster = CBPS_data$New_geog_code, test = "Satterthwaite")$p
  FEstabeconsum_cbps$tidy$std.error <- coef_test(FEstabecon_cbps, vcov = "CR2", cluster = CBPS_data$New_geog_code, test = "Satterthwaite")$SE
  FEstabeconsum_cbps$tidy$conf.low <- FEstabeconsum_cbps$tidy$estimate-(1.96*coef_test(FEstabecon_cbps, vcov = "CR2", cluster = CBPS_data$New_geog_code, test = "Satterthwaite")$SE)
  FEstabeconsum_cbps$tidy$conf.high <- FEstabeconsum_cbps$tidy$estimate+(1.96*coef_test(FEstabecon_cbps, vcov = "CR2", cluster = CBPS_data$New_geog_code, test = "Satterthwaite")$SE)
  FEstabeconsum_cbps$tidy$estimate <- FEstabeconsum_cbps$tidy$estimate
  
  # FEmisssum$tidy$p.value <- coef_test(FEmiss, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$p
  # FEmisssum$tidy$std.error <- coef_test(FEmiss, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE
  # FEmisssum$tidy$conf.low <- FEmisssum$tidy$estimate-(1.96*coef_test(FEmiss, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE)
  # FEmisssum$tidy$conf.high <- FEmisssum$tidy$estimate+(1.96*coef_test(FEmiss, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE)
  # FEmisssum$tidy$estimate <- FEmisssum$tidy$estimate
  # 
  # FEmissconsum$tidy$p.value <- coef_test(FEmisscon, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$p
  # FEmissconsum$tidy$std.error <- coef_test(FEmisscon, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE
  # FEmissconsum$tidy$conf.low <- FEmissconsum$tidy$estimate-(1.96*coef_test(FEmisscon, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE)
  # FEmissconsum$tidy$conf.high <- FEmissconsum$tidy$estimate+(1.96*coef_test(FEmisscon, vcov = "CR2", cluster = df$New_geog_code, test = "Satterthwaite")$SE)
  # FEmissconsum$tidy$estimate <- FEmissconsum$tidy$estimate
  
  
  #feonly
  
  cm <- c("per_for_profit" = "For-profit Outsourcing (%)",
          "per_foster_percent" = "Fostering placements (%)",
          "per_white_percent" = "CIC ethnicity (white, %)",
          "per_female_percent" = "CIC sex (Female, %)",
          "CLA_Mar" = "CIC (n)", 
          "per_short_term" = "Short term only placements (%)",
          "Total_spend" = "Children's Social Care Expenditure (£, Ms)")
  
  rows <- tribble(~term,          ~`Placements outside LA (%) [.95 ci]`,  ~`p-value`,~`Placements outside LA (%) [.95 ci]`,  ~`p-value`,  ~`Placements outside LA (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`,  ~`Placements unstable (%) [.95 ci]`,  ~`p-value`, 
                  'CCG Fixed Effects', 'Yes',  'Yes', 'Yes',  'Yes','Yes',  'Yes','Yes',  'Yes',  'Yes','Yes','Yes','Yes',
                  'Time Fixed Effects','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',  'Yes',  'Yes','Yes','Yes','Yes',
                  'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',  'Yes')
  
  
  table <- modelsummary(list("Placements outside LA [.95 ci]"=FElocsum,"p-value"=FElocsum,"Placements outside LA [.95 ci]"=FElocconsum,"p-value"=FElocconsum,"Placements outside LA [.95 ci]"=FElocconsum_cbps,"p-value"=FElocconsum_cbps, "Placements unstable (%) [.95 ci]" = FEstabesum, "p-value" = FEstabesum,"Placements unstable (%) [.95 ci]" = FEstabeconsum, "p-value" = FEstabeconsum,"Placements unstable (%) [.95 ci]" = FEstabeconsum_cbps, "p-value" = FEstabeconsum_cbps),
                        coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm,
                        fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                        notes = list('Table reports results from multivariate longitudinal regression models.',
                                     'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)'),
                        output = "gt") 
  # add_header_above(c(" ", "Fixed Effects" = 2, "First Differences" = 2, "Covariate Balancing (1)" = 2, "Covariate Balancing (2)" = 2, "Multi-Level Model" = 2))
  
table
  
}