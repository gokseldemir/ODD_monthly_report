########################################IGNORE##########################################
###################CRUDE ONLY#####################

####FOR AGE STANDARDIZED RATES START AT LINE 60!
pop$FNCF_MATCH <- as.character(pop$FNCF_MATCH)

ha_mortality_ytd <- region%>% 
      select(-ha_name) %>% 
  rename("year" = DATEOFDEATH_YEAR, 
         "ha_id" = FINAL_DERIVED_HA) %>% 
  mutate(FNCF_MATCH = as.character(if_else(FirstNations == "first_nations", 1, 0))) %>%
  #mutate(year = 2020) %>% 
  left_join(pop, by = c("ha_id", "year", "FNCF_MATCH"))  %>%  
  mutate(deathrate = 100000*total/Population)

##create subset for small numerators (based on guidelines from Washington State Public Health Dept)
ha_mortality_daly <- ha_mortality_ytd  %>% filter(total<100)

#create subset for larger numerators
ha_mortality_approx <- ha_mortality_ytd %>% filter(total >= 100)

###Confidence intervals 
ha_mortalitylimits_daly <- pois.daly(ha_mortality_daly$total, ha_mortality_daly$Population) %>% 
  mutate(LL = lower*100000, UL = upper*100000) %>% 
  rename(total = x, Population = pt) 

ha_mortalitylimits_approx <- pois.approx(ha_mortality_approx$total, ha_mortality_approx$Population) %>% 
  mutate(LL = lower*100000, UL = upper*100000) %>% 
  rename(total = x, Population = pt)  

ha_mortalityLimits <- bind_rows(ha_mortalitylimits_daly, ha_mortalitylimits_approx) %>% 
  right_join(ha_mortality_ytd, by= c("total", "Population"))

ha_export_crude_rates <- ha_mortalityLimits %>% 
  select(c(ha_name.x, ha_id, year, FirstNations, total, Population, deathrate, LL, UL, conf.level))

ha_export_crude_rates <- ha_export_crude_rates %>%  
  mutate(deathrateproj = ifelse(year == 2020, deathrate*12/12, deathrate), 
         ULproj = ifelse(year == 2020, UL*12/12, UL), 
         LLproj = ifelse(year==2020, LL*12/12, LL))


ggplot(ha_export_crude_rates, aes(x=ha_name.x, y=deathrateproj, fill=FirstNations)) +  #map variables on x, y, bygroup
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LLproj, ymax=ULproj), width = 0.3, position = position_dodge(1)) +#make bars and make them side by side 
  scale_fill_manual(values = c("darkorange1", "skyblue4"), name = NULL, labels = c("First Nations", "Other Residents")) +
  standards+
  geom_text(aes(label=round(..y.., digits = 1)), size = 4.5, position = position_dodge(width = 1), vjust=-0.8, hjust=1.0) +
  labs(title = paste("Overdose Mortality Rates: First Nations & Other Residents"), 
       subtitle = "BC",
       x = "HA",  y = "projected rate per 100,000") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

write_csv(ha_export_crude_rates, path=paste0(folder, "provincial_rates_gender_out_", Sys.Date(), ".csv"))

# ggsave(file=here(reporting_mth, "POP_UPDATE_HA_YTD_crude_mortality_rate.jpg"),  width = 7.16, height = 6.8)
ggsave("POP_UPDATE_HA_YTD_crude_mortality_rate.jpg", path = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/September", width = 7.16, height = 6.8)


#############################################################################
##Age Standardized Rats by Region###
# bc_ha_age_pop<- read_csv(file = "L:/Health Surveillance/Health Surveillance - General/lookup_files/r_readin_bc_fn18_pop_ha_age.csv")
# can_std_pop <- read_csv(file = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/r_readin_canadian_std_pop.CSV")


bc_ha_age_pop<- read_csv(file = "//fnha.local/groups/Health Surveillance/Health Surveillance - General/lookup_files/r_readin_bc_fn18_pop_ha_age.csv")
can_std_pop <- read_csv(file = "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/r_readin_canadian_std_pop.CSV")


std_pop_cl <- can_std_pop %>% mutate(age_group_stds =  ifelse(age_group %in% "0 to 4 years"| age_group %in% "5 to 9 years", "<10", 
                                                              ifelse(age_group %in% "10 to 14 years"| age_group %in% "15 to 19 years", "10-19",
                                                                     ifelse(age_group %in% "20 to 24 years" | age_group %in% "25 to 29 years", "20-29", 
                                                                            ifelse(age_group %in% "30 to 34 years" | age_group %in% "35 to 39 years", "30-39", 
                                                                                   ifelse(age_group %in% "40 to 44 years" | age_group %in% "45 to 49 years", "40-49", 
                                                                                          ifelse(age_group %in% "50 to 54 years" | age_group %in% "55 to 59 years", "50-59",
                                                                                                 ifelse(age_group %in% "60 to 64 years" | age_group %in% "65 to 69 years", "60-69",
                                                                                                        "70+")))))))) %>% 
  group_by(age_group_stds) %>%  
  summarise(pop=sum(population, na.rm=T)) %>% arrange(age_group_stds)



bccs <- bccs %>% mutate(age_group_stds = ifelse(DECEASED_AGE <10, "<10",
                                                ifelse((DECEASED_AGE >=10 & DECEASED_AGE <20), "10-19",
                                                ifelse((DECEASED_AGE >= 20 & DECEASED_AGE <30), "20-29", 
                                                       ifelse((DECEASED_AGE >=30 & DECEASED_AGE <40), "30-39",
                                                              ifelse((DECEASED_AGE >=40 & DECEASED_AGE <50), "40-49", 
                                                                     ifelse((DECEASED_AGE >=50 & DECEASED_AGE <60), "50-59", 
                                                                            ifelse((DECEASED_AGE >=60 & DECEASED_AGE <70),  "60-69",
                                                                                   ifelse(DECEASED_AGE >=70, "70+",
                                                                                          NA)))))))))

by_age_ha_stds <- bccs %>% 
  group_by(DATEOFDEATH_YEAR, FINAL_DERIVED_HA, FNCF_MATCH ,age_group_stds) %>% 
  summarise(total_deaths = n()) %>% 
  mutate("Region" = as.character(FINAL_DERIVED_HA), "Year" = as.character(DATEOFDEATH_YEAR)) 


bc_fn_pop_tw <- bc_ha_age_pop %>% 
  filter(Year == 2019) %>% 
  mutate(Year = 2020)

bc_fn_pop_tw_o <- bc_ha_age_pop %>% 
  filter(Year == 2019) %>% 
  mutate(Year = 2021)

bc_ha_age_pop_cl <- bind_rows(bc_ha_age_pop, bc_fn_pop_tw,bc_fn_pop_tw_o) %>% 
  arrange(Year, Region, fn_flag, age_group_stds) %>% 
  filter(!is.na(age_group_stds)) %>% 
  mutate( Region = as.character(Region), 
         Year = as.character(Year), 
         fn_flag_new = ifelse(fn=="first_nations", "1", "0")) 

region<- unique(by_age_ha_stds$Region)
fn_lab <- unique(bc_ha_age_pop_cl$fn_flag_new)
years <- unique(by_age_ha_stds$Year)

bc_ha_age_pop

region_adjusted_rate <- data.frame()

all_ages <- tibble(age_group_stds = unique(by_age_ha_stds$age_group_stds), total_deaths = 0) %>% arrange(age_group_stds)

for (i in seq_along(region)) {
  for (j in seq_along(years)) { 
    for (k in seq_along(fn_lab)) {
      
      subset <-  subset(by_age_ha_stds, Region == region[i] & DATEOFDEATH_YEAR == years[j] & FNCF_MATCH == fn_lab[k]) 
      fn_pop <-  subset(bc_ha_age_pop_cl, Region == region[i] & Year == years[j] & fn_flag_new == fn_lab[k])
      std_pop <- std_pop_cl
      
      
      if (nrow(subset) < nrow(all_ages)) {
        subset <- left_join(all_ages, subset, by="age_group_stds")
        subset$year <- years[j]
        subset$total_deaths = ifelse(is.na(subset$total_deaths.y) == T, 0, subset$total_deaths.y)
      }
      else {
        subset <- subset
      }
      
      
      adjust_year_i <- broom::tidy(epitools::ageadjust.direct(count = subset$total_deaths, pop=fn_pop$population, stdpop = std_pop$pop)) %>%
        mutate(Year=years[j],  Region = region[i],       fn_flag = fn_lab[k])
      
      region_adjusted_rate <-bind_rows(region_adjusted_rate, adjust_year_i)
      
      print(paste("Rates for specific cause of death:", region[i], years[j], fn_lab[k]))
      print(adjust_year_i)
    }
  }
}


region_adjusted_wide <- region_adjusted_rate %>% 
  mutate(per_hundred_thousand_x = x*100000) %>% select(-x) %>% 
  pivot_wider(names_from = names, 
              values_from = per_hundred_thousand_x) %>% 
  arrange(Year, Region, desc(adj.rate)) %>% 
  mutate(crude_proj = ifelse(Year == "2021", 12/month_num*crude.rate, crude.rate), 
         adj_proj = ifelse(Year == "2021", 12/month_num*adj.rate, adj.rate), 
         lci_proj = ifelse(Year == "2021", 12/month_num*lci, lci), 
         uci_proj = ifelse(Year == "2021", 12/month_num*uci, uci))

ha_export_cy <- region_adjusted_wide %>% 
  filter(Year == 2021) %>% 
  mutate(ha_name = ifelse(Region == 1, "Interior", 
                          ifelse(Region == 2, "Fraser", 
                                 ifelse(Region == 3, "Vancouver Coastal", 
                                        ifelse(Region ==4, "Vancouver Island", 
                                               ifelse(Region ==5, "Northern", NA))))), 
         fn = ifelse(fn_flag =="1", "first_nations", "other_residents"))
ggplot(ha_export_cy, aes(x=ha_name, y=adj_proj, fill=fn)) +  #map variables on x, y, bygroup
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lci_proj, ymax=uci_proj), width = 0.3, position = position_dodge(1)) +#make bars and make them side by side 
  scale_fill_manual(values = c("darkorange1", "skyblue4"), name = NULL, labels = c("First Nations", "Other Residents")) +
  standards+
  geom_text(aes(label=round(..y.., digits = 1)), size = 4.5, position = position_dodge(width = 1), vjust=-0.8, hjust=1.0) +
  labs(title = paste(" Age Standardised Overdose Mortality Rates: First Nations & Other Residents"), 
       subtitle = "BC",
       x = "HA",  y = "rate per 100,000, age-standardized") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave(here(folder, file="pop_update_age_std_region_specifc_rates.jpg"), width = 8.85, height = 7.0)
ggsave("pop_update_age_std_region_specifc_rates.jpg", path = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/September", width = 8.85, height = 7.0)

