###############################
##

library(tidyverse)
library(scales)
library(epitools)
library(odbc)
library(dbplyr)
library(lubridate)
library(broom)
library(ggrepel)
library(flextable)
library(readxl)
library(here)

reporting_mth <- "April"
folder_year <- 2022
folder <- "April"

dbhandle <- dbConnect(odbc(), driver="ODBC Driver 17 for SQL Server", server="FNHADW", database="CHWSDM", Trusted_Connection = "yes")
tester <- tbl(dbhandle, in_schema("dbo", "F_BCCS_Illicit_Drug_ODs_FN_Deid"))
#tester <- tester %>% select(MOH_STUDYID, OPEN_OR_CLOSED, DECEASED_AGE, FENTANYL_DETECTED, FINAL_DERIVED_HA, FINAL_DERIVED_HSDA, FINAL_DERIVED_LHA, Final_Derived_LHA18, DATEOFINJURY_YEAR, DATEOFDEATH, DATEOFDEATH_YEAR, F1_TOXISUBTYPEDESCRI:Q12_OTHER_SPECIFY, GENDER, FNCF_MATCH) 
bccs <- as_tibble(tester)

format_dates <- function(x) {
  months <-  month(x, label = T, abbr=T)            
  years <- lubridate::year(x) 
  
  if_else(is.na(lag(years)) | lag(years) != years,  
          true = paste(years, months, sep = " - "), 
          false = paste(months))
}

standards <- theme_classic() + 
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        panel.grid.major = element_blank(), 
        panel.grid.minor=element_blank(),
        axis.title = element_text(size=16),
        legend.title = element_blank())


bccs <- bccs %>% 
  filter(year(DATE_OF_DEATH) > 2015) %>% 
  mutate(ha_name = ifelse(FINAL_DERIVED_HA == 1, "Interior", 
                          ifelse(FINAL_DERIVED_HA == 2, "Fraser",
                                 ifelse(FINAL_DERIVED_HA == 3, "Vancouver Coastal",
                                        ifelse(FINAL_DERIVED_HA ==4, "Vancouver Island",
                                               ifelse(FINAL_DERIVED_HA == 5, "Northern", "Unknown"))))),
         FirstNations = ifelse(FNCF_MATCH == 1, "first_nations", "other_residents"),
         DATE_OF_DEATH = ymd(DATE_OF_DEATH),
         dateofdeath_floor = floor_date(DATE_OF_DEATH, unit="month"),
         month = month(DATE_OF_DEATH),
         month_lab = month(DATE_OF_DEATH, label = T, abbr = T),
         quarter = ifelse(month < 4, "Q1",
                          ifelse(month >=4 & month <7, "Q2",
                                 ifelse(month >=7 & month <10, "Q3",
                                        ifelse(month >=10 & month <13, "Q4", NA)))),
         year_quarter = paste(year(DATE_OF_DEATH), quarter, sep="-"), 
         DATEOFDEATH_YEAR = year(DATE_OF_DEATH),
         age_group = ifelse(DECEASED_AGE <20, "<20",
                            ifelse(DECEASED_AGE >=20  &  DECEASED_AGE <=29, "20-29",
                                   ifelse(DECEASED_AGE>=30 &  DECEASED_AGE<=39, "30-39",
                                          ifelse(DECEASED_AGE >=40 &  DECEASED_AGE<= 49, "40-49",
                                                 ifelse(DECEASED_AGE >=50 &  DECEASED_AGE<= 59, "50-59",
                                                        ifelse(DECEASED_AGE >=60 , ">=60", "Unknown")))))) 
  ) 
#filter(DATEOFDEATH <= ymd(params$subset_end_date))

bccs <- unique(bccs)
length(unique(bccs$MOH_STUDYID))

by_year <- bccs %>% 
  group_by(DATEOFDEATH_YEAR, FirstNations) %>% 
  summarise(total = n(), n_quarters = n_distinct(quarter)/4) %>% 
  mutate(percent=round(total/sum(total), digits=3),
         ylabs = as.character(ifelse(n_quarters < 1, paste0(DATEOFDEATH_YEAR, "*"), DATEOFDEATH_YEAR)), 
         n_quarters = (ifelse(n_quarters < 0.5, 0.37, n_quarters))
  )
by_year_m <- bccs %>% 
  group_by(dateofdeath_floor, FirstNations) %>% 
  summarise(total_deaths = n_distinct(MOH_STUDYID)) %>% 
  mutate(percent = round(total_deaths/sum(total_deaths), digits = 3), 
         FirstNations_f = fct_relevel(FirstNations, "other_residents"))

fn_labs <- c("other_residents"= "Other Residents", "first_nations" = "First Nations")

one_yr_roll <- seq(max(bccs$dateofdeath_floor), by="-1 month", length = 12)
one_yr_roll

by_year_m %>% 
  filter(FirstNations == "first_nations") %>%
  filter(dateofdeath_floor %in% one_yr_roll) %>%
  ggplot(aes(dateofdeath_floor, total_deaths, fill=FirstNations_f)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=total_deaths), vjust=1.6, color="white",
            position = position_dodge(width=28), size=4.5) +
  #facet_grid(FirstNations_f~., scale="free", labeller = labeller(FirstNations_f=fn_labs))+
  scale_x_date(labels = format_dates, date_breaks = "1 month", expand = c(0,0)) +
  scale_fill_manual(values = c("darkorange1"), name=NULL, labels = c("First Nations", "Other Residents")) +
  standards + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle=45, hjust=1), 
        strip.text = element_text(size=14))+ 
  labs(title = "2020 Drug Poisoning Deaths by Month", 
       subtitle = "First Nations & Other Residents, BC",
       x = "Month",  y = "Total OD Deaths")

ggsave(here(reporting_mth, "OD_deaths_by_month.jpg"), width =  15, height = 6.5)


by_year %>% 
  filter(FirstNations == "first_nations") %>%  
  ggplot(aes(ylabs, total)) +
  geom_bar(stat="identity", position=position_dodge(), fill = "darkorange1") +
  geom_text(aes(label=total), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4.5) +
  #scale_fill_manual(values = "darkorange1", name=NULL) +
  standards + 
  theme(legend.position = "bottom")+ 
  labs(title = "Drug Poisoning Deaths by Year", 
       subtitle = "First Nations",
       x = "Year",  y = "Total OD Deaths")


ggsave(here(reporting_mth, "OD_deaths_by_YR.jpg"), width =  10, height = 6.5)

percent_fn_2020 <- by_year_m %>% filter(year(dateofdeath_floor) >= 2020)
 percent_fn_2020 %>% 
  mutate(FirstNations = fct_relevel(FirstNations, "other_residents", "first_nations")) %>% 
  ggplot(aes(dateofdeath_floor, total_deaths, fill=FirstNations, width = 25)) + 
  geom_bar(stat = "identity", position = "fill")  +  
  geom_text(aes(label=paste0("n=", total_deaths)), position = position_fill(vjust = 0.5), size = 4.5) + 
  geom_text(data = subset(percent_fn_2020, FirstNations == "first_nations"), 
            aes(x = dateofdeath_floor, y = percent, label = paste0(100*percent, "%")), 
            color = "darkorange1", size = 5.5, fontface="bold", nudge_y = 0.035) +
  scale_x_date(labels = format_dates, date_breaks = "1 month", expand = c(0,0)) +
  scale_fill_manual(values = c("skyblue4", "darkorange1"), name=NULL, labels = c("Other Residents", "First Nations")) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.5)) + 
  standards +
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(title = "First Nations OD deaths as a percentage of all OD Deaths", 
       subtitle = "BC",
       x = "Month",  y = "Percentage")

 ggsave(here(reporting_mth, "stack_chart.jpg"), width = 13.4, height = 7.66)

 region <- bccs %>% 
   filter(DATEOFDEATH_YEAR >= 2021) %>% 
   group_by(DATEOFDEATH_YEAR, FINAL_DERIVED_HA, ha_name, FirstNations) %>% 
   summarise(total = n())

region %>% 
   ggplot(aes(ha_name, total, fill=FirstNations)) +
   geom_bar(stat="identity", position=position_dodge()) +
   geom_text(aes(label=total), vjust=-0.7, color="black",
             position = position_dodge(0.9), size=4.5) +
  facet_wrap(DATEOFDEATH_YEAR~.)+
   scale_fill_manual(values = c("darkorange1", "skyblue4"), name=NULL, labels = c("First Nations", "Other Residents")) +
   standards + 
   theme(legend.position = "bottom", 
         strip.text = element_text(size=16), 
         axis.text.x = element_text(angle=45, hjust=1))+ 
   labs(title = "Drug Poisoning Deaths by Health Authority", 
        subtitle = "First Nations & Other Residents, BC",
        x = "Health Authority",  y = "Total OD Deaths")


ggsave(here(reporting_mth, "OD_deaths_by_region.jpg"), width =10.5 , height = 8)
#ggsave(here(reporting_mth,"OD_deaths_by_region_current_month.jpg"), width =9.5 , height = 7)


by_sex_year <- bccs %>% filter(DATEOFDEATH_YEAR > 2015) %>% 
  mutate(Gender = fct_relevel(GENDER, "Male", "Female", "Unknown")) %>% 
  group_by(DATEOFDEATH_YEAR, FirstNations, GENDER) %>% 
  summarise(deaths = n(),
            n_quarters = n_distinct(quarter)/4) %>%  
  mutate(percent = 100*deaths/sum(deaths), 
         ylabs = as.character(ifelse(n_quarters < 1, paste0(DATEOFDEATH_YEAR, "*"), DATEOFDEATH_YEAR)), 
         n_quarters = (ifelse(n_quarters < 0.5, 0.43, n_quarters))) 


by_sex_year <- by_sex_year %>%  filter(FirstNations == "first_nations") 

sex_labels <- c(first_nations = "First Nations", other_residents = "Other Residents")

by_sex_year %>%  filter(GENDER != "Unknown") %>% 
  ggplot(aes(ylabs, deaths, fill = GENDER, width = n_quarters/1.1)) + 
  geom_bar(stat = "identity", position = "fill") + #make bars and make them side by side 
  scale_fill_manual(values = (c("lightgoldenrod1", "darkslateblue")), guide=guide_legend(reverse = F)) + 
  facet_grid(.~FirstNations, labeller=labeller(FirstNations=sex_labels)) +
  standards +
  geom_text(aes(label=paste0(round(percent, digits = 1), "%", "\n", "n=", deaths)), 
            position = position_fill(vjust = 0.5), size = 4.5,
            colour=ifelse(subset(by_sex_year, GENDER != "Unknown")$GENDER == "Female", "black", "gray")) + 
  theme(legend.position = "right", 
        strip.background = element_rect(fill="white"), 
        strip.text.x = element_text(
          size = 14, color = "black", face = "bold")) +
  #scale_x_discrete(labels = c("First Nations", "Other Residents")) + #add labels
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.5)) +
  labs(title = "Drug Poisoning Deaths by Sex", 
       subtitle = "BC",
       x = " ", y="Percentage")

by_sex_year %>%  filter(GENDER != "Unknown") %>% 
  ggplot(aes(ylabs, deaths, fill = GENDER, width = n_quarters/1.1)) + 
  geom_bar(stat = "identity", position = "fill") + #make bars and make them side by side 
  scale_fill_manual(values = (c("lightgoldenrod1", "slateblue2")), guide=guide_legend(reverse = F)) + 
  facet_grid(.~FirstNations, labeller=labeller(FirstNations=sex_labels)) +
  standards +
  geom_text(aes(label=paste0(round(percent, digits = 1), "%", "\n", "n=", deaths)), 
            position = position_fill(vjust = 0.5), size = 4.5,
            colour=ifelse(subset(by_sex_year, GENDER != "Unknown")$GENDER == "Female", "black", "black")) + 
  theme(legend.position = "right", 
        strip.background = element_rect(fill="white"), 
        strip.text.x = element_text(
          size = 14, color = "black", face = "bold")) +
  #scale_x_discrete(labels = c("First Nations", "Other Residents")) + #add labels
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.5)) +
  labs(title = "Drug Poisoning Deaths by Sex", 
       subtitle = "BC",
       x = " ", y="Percentage")
ggsave(here(reporting_mth, "adhoc_sex_by_year.jpg"), width = 12.85, height = 7.8)


pop <- pop <- read_csv(file="L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/R_readin_2020.08.05_Population_File.csv")



#
pop<- pop %>% 
  rename("FNCF_MATCH" = FirstNations)

pop$FNCF_MATCH <-as.character(pop$FNCF_MATCH)

mortality_ratetoap <- percent_fn_2020 %>% 
  dplyr::mutate(ha_id = 6, 
         FNCF_MATCH = as.character(if_else(FirstNations == "first_nations", 1, 0))) %>%
  dplyr::mutate(year = 2020) %>% 
  left_join(pop, by = c("ha_id", "year", "FNCF_MATCH"))  %>%  
  mutate(deathrate = 100000*total_deaths/Population)

mortality_ytd <- by_year %>% 
  rename("year" = DATEOFDEATH_YEAR) %>% 
  mutate(ha_id = 6, 
         FNCF_MATCH = as.character(if_else(FirstNations == "first_nations", 1, 0))) %>%
  #mutate(year = 2020) %>% 
  left_join(pop, by = c("ha_id", "year", "FNCF_MATCH"))  %>%  
  mutate(deathrate = 100000*total_deaths/Population)

mortality_daly <- mortality_ratetoap  %>% filter(total_deaths <100)

mortality_approx <- mortality_ratetoap  %>% filter(total_deaths >= 100)

###Confidence intervals 
ha_mortalitylimits_daly <- pois.daly(mortality_daly$total_deaths, mortality_daly$Population) %>% 
  mutate(LL = lower*100000, UL = upper*100000) %>% 
  rename(total_deaths = x, Population = pt) 

ha_mortalitylimits_approx <- pois.approx(mortality_approx$total_deaths, mortality_approx$Population) %>% 
  mutate(LL = lower*100000, UL = upper*100000) %>% 
  rename(total_deaths = x, Population = pt)  

ha_mortalityLimits <- bind_rows(
  ha_mortalitylimits_daly,
 ha_mortalitylimits_approx) %>% 
right_join(mortality_ratetoap, by= c("total_deaths", "Population"))

ha_export_crude_rates <- ha_mortalityLimits %>% 
  select(c(ha_id, year, dateofdeath_floor, FirstNations, FNCF_MATCH, total_deaths, Population, deathrate, LL, UL, conf.level))
ha_export_crude_rates <- ha_export_crude_rates %>%
  filter(dateofdeath_floor >= ymd("2021-01-01"))

ggplot(ha_export_crude_rates, aes(x=dateofdeath_floor, y=deathrate, fill=FirstNations)) +  #map variables on x, y, bygroup
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LL, ymax=UL), width = 7, position = position_dodge(28)) +#make bars and make them side by side 
  scale_fill_manual(values = c("darkorange1", "skyblue4"), name = NULL, labels = c("First Nations", "Other Residents")) +
  standards+
  scale_x_date(labels=format_dates, expand=c(0.025,0), date_breaks = "1 month")+
  geom_text(aes(label=round(..y.., digits = 1)), size = 4.5, position = position_dodge(width = 28), vjust=-0.8, hjust=1.0) +
  labs(title = paste("Drug Poisoning Mortality Rates: First Nations & Other Residents"), 
       subtitle = "BC",
       x = "Month",  y = "rate per 100,000") +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle=45, hjust=1))

ggsave(file=here(reporting_mth, "POP_UPDATE_MONTHLY_crude_mortality_rate.jpg"),  width = 14.16, height = 6.8)
# ggsave("POP_UPDATE_MONTHLY_crude_mortality_rate3.jpg", path = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/July", width = 14.16, height = 6.8)


band_lookup <- read_csv(file = "L:/Health Surveillance/Health Surveillance - General/lookup_files/r_readin_final_lookup_geography.csv")
lha18 <- band_lookup %>% 
  select(lha_2018, lha_name_2018, hsda_name, ha_name) %>% 
  rename("FINAL_DERIVED_LHA18" = lha_2018) %>% unique()


#####################################################################
###############IGNORE THIS############################################

mortality_rate <- by_year %>% 
  mutate(ha_id = 6, 
         FNCF_MATCH = as.character(if_else(FirstNations == "first_nations", 1, 0))) %>% 
  rename(year = DATEOFDEATH_YEAR) %>% 
  left_join(pop, by = c("ha_id", "year", "FNCF_MATCH"))  %>%  
  mutate(deathrate = 100000*total/Population)

mortality_daly <- mortality_rate  %>% filter(total <100)

mortality_approx <- mortality_rate  %>% filter(total >= 100)




###Confidence intervals 


mortalitylimits_daly <- pois.daly(mortality_daly$total, mortality_daly$Population) %>% 
  mutate(LL = lower*100000, UL = upper*100000) %>% 
  rename(total = x, Population = pt) 

mortalitylimits_approx <- pois.approx(mortality_approx$total, mortality_approx$Population) %>% 
  mutate(LL = lower*100000, UL = upper*100000) %>% 
  rename(total = x, Population = pt)  

mortalityLimits <- bind_rows(
  mortalitylimits_daly,
  mortalitylimits_approx) %>% 
  right_join(mortality_rate, by= c("total", "Population"))

mortalityLimits <- mortalitylimits_approx %>% 
  right_join(mortality_rate, by= c("total", "Population"))
export_crude_rates <- mortalityLimits %>%  
  select(c(ha_id, ha_name, year, ylabs, FNCF_MATCH, FirstNations, total, Population, deathrate, LL, UL, conf.level))

write_csv(export_crude_rates, "POP_UPDATED_crude_mortality_rates.csv")

ggplot(export_crude_rates, aes(x=ylabs, y=deathrate, fill=FirstNations)) +  #map variables on x, y, bygroup
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LL, ymax=UL), width = 0.3, position = position_dodge(1)) +#make bars and make them side by side 
  scale_fill_manual(values = c("darkorange1", "skyblue4"), name = NULL, labels = c("First Nations", "Other Residents")) +
  standards+
  geom_text(aes(label=round(..y.., digits = 1)), size = 4.5, position = position_dodge(width = 1), vjust=-0.8, hjust=1.0) +
  labs(title = paste("Overdose Mortality Rates: First Nations & Other Residents"), 
       subtitle = "BC",
       x = "Year",  y = "rate per 100,000") +
  theme(legend.position = "bottom")
ggsave(file="POP_UPDATED_YEARLY_RATES_Fig5_crude_mortality_rate.jpg",  width = 8.16, height = 7)

#######################################################

ggsave(file = "POP_UPDATED_rates2020todate.jpg", width = 6.16, height = 6.3)

lha <-bccs %>%
  # filter(DATE_OF_DEATH <= ymd("2021-06-01"))%>%
  group_by(DATEOFDEATH_YEAR, FINAL_DERIVED_LHA18, FirstNations) %>% 
  summarise(total = n()) %>% 
  arrange(DATEOFDEATH_YEAR, desc(total)) %>% 
  left_join(lha18, by=c("FINAL_DERIVED_LHA18")) %>% 
  select(ha_name, hsda_name, FINAL_DERIVED_LHA18, lha_name_2018, everything())

lha_out <- lha %>% 
  unite(temp, DATEOFDEATH_YEAR, FirstNations) %>% 
  pivot_wider(names_from = temp,
              values_from = total, 
              values_fill = 0) %>% 
  arrange(desc(`2020_first_nations`))

write_csv(lha_out, file = here(reporting_mth, paste("lhas_to", reporting_mth, folder_year, ".csv", sep="_")))

#################################################################################
#### Home & Away from Home 
#############################################
band_lha <- band_lookup %>% 
  select(band, lha_2018, lha_name_2018) %>% 
  mutate(band = as.character(band)) %>% 
  unique()

bccs$FNCF_GROUP_CODE = as.character(bccs$FNCF_GROUP_CODE)

bccs_band <- bccs %>% filter(FNCF_MATCH == 1)  %>% 
  rename("band" = FNCF_GROUP_CODE) %>%
  left_join(band_lha, by="band") %>% 
  mutate(home_nd_away_new = ifelse(is.na(lha_2018) == T, 'AWAY', 
                                   ifelse(lha_2018 == FINAL_DERIVED_LHA18, 'HOME', 'AWAY')))   %>% 
  select(MOH_STUDYID, DATEOFDEATH_YEAR, GENDER, home_nd_away_new, band, FINAL_DERIVED_LHA18, FINAL_DERIVED_HA, ha_name, lha_2018, lha_name_2018)

year_band <- bccs_band %>% 
  group_by(DATEOFDEATH_YEAR, ha_name, home_nd_away_new) %>% 
  summarise(total=n())

year_band_bc <- bccs_band %>% 
  group_by(DATEOFDEATH_YEAR, home_nd_away_new) %>% 
  summarise(total=n()) %>% mutate(ha_name = 'BC')

home_away_out <- bind_rows(year_band, year_band_bc) %>% mutate(percent = total/sum(total))


write_csv(home_away_out, file = here(reporting_mth, "home_and_away_out.csv"))
# write_csv(home_away_out, file = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/June/home_and_away_out.csv")
  

lha_home_bands <- bccs_band %>% filter(home_nd_away_new == 'HOME') %>% group_by(DATEOFDEATH_YEAR, lha_name_2018) %>% summarise(total=n())

  #######################LHAs for same time period ####IGNORE THIS######
  
  lha <-bccs %>% filter(month <=8) %>% 
    group_by(DATEOFDEATH_YEAR, Final_Derived_LHA18, FirstNations) %>% 
    summarise(total = n()) %>% 
    arrange(DATEOFDEATH_YEAR, desc(total)) %>% 
    left_join(lha18, by=c("Final_Derived_LHA18")) %>% 
    select(ha_name, hsda_name, Final_Derived_LHA18, lha_name_2018, everything())
  
  lha_out <- lha %>% 
    unite(temp, DATEOFDEATH_YEAR, FirstNations) %>% 
    pivot_wider(names_from = temp,
                values_from = total, 
                values_fill = 0) %>% 
    arrange(desc(`2020_first_nations`))

write_csv(lha_out, path = "lhas_jantoaug_everyyr.csv")  
######################################################
####### Average ages
for (i in (2019:2021)) {
  for (j in c("first_nations", "other_residents")) {
    print(paste("The average of", j, "in", i, "was", 
                (mean(bccs$DECEASED_AGE[bccs$FirstNations == j & bccs$DATEOFDEATH_YEAR == i], na.rm = T))))
  }
}

####Age group 
bccs <- bccs %>% mutate(age_group = ifelse(DECEASED_AGE <20, "<20",
                                           ifelse((DECEASED_AGE >= 20 & DECEASED_AGE <30), "20-29", 
                                                  ifelse((DECEASED_AGE >=30 & DECEASED_AGE <40), "30-39",
                                                         ifelse((DECEASED_AGE >=40 & DECEASED_AGE <50), "40-49", 
                                                                ifelse((DECEASED_AGE >=50 & DECEASED_AGE <60), "50-59", 
                                                                       ifelse(DECEASED_AGE >=60, "60+", NA)))))))


age_HA_FN <- bccs %>% 
  filter(dateofdeath_floor %in% one_yr_roll) %>%
  group_by(FirstNations, FNCF_MATCH, age_group) %>% 
  summarise(deaths = n()) %>%
  mutate(percent = deaths/sum(deaths))

ggplot(age_HA_FN, aes(x=age_group, y=percent, fill = FirstNations)) +  
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c( "darkorange1", "skyblue4"), labels = c("First Nations", "Other Residents")) +
  scale_y_continuous(limits = c(0, .4), breaks = seq(0, .4, by=.2), labels = scales::percent ) +
  standards + 
  facet_grid(.~FirstNations)+
  theme(legend.position = "bottom", 
        strip.text.x = element_text(size = 14)) +
  geom_text(aes(label=paste(round(100*percent, digits = 1), "%", "\n", "n=", deaths)), 
            size = 4.5, position = position_dodge(width = 1), vjust=-0.45) + 
  labs(title = "Drug Poisoning Deaths by Age Group One Year Rolling", 
       subtitle = "BC",
       x = "Age Group", y="Percentage")


ggplot(age_HA_FN, aes(x=age_group, y=percent, fill = FirstNations)) +  
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c( "darkorange1", "skyblue4"), labels = c("First Nations", "Other Residents")) +
  scale_y_continuous(limits = c(0, .4), breaks = seq(0, .4, by=.2), labels = scales::percent ) +
  standards + 
  facet_grid(.~DATEOFDEATH_YEAR)+
  theme(legend.position = "bottom", 
        strip.text.x = element_text(size = 14)) +
  geom_text(aes(label=paste(round(100*percent, digits = 1), "%", "\n", "n=", deaths)), 
            size = 4.5, position = position_dodge(width = 1), vjust=-0.45) + 
  labs(title = "Drug Poisoning Deaths by Age Group", 
       subtitle = "BC",
       x = "Age Group", y="Percentage")

ggsave(here(reporting_mth, file="age_by_year.jpg"), width = 15.85, height = 7.8)
# ggsave(file = "age_HA_FN, device.png", path = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/July" , width = 15.85, height = 7.8) 




################################################################
##month and region
##month and region
by_month_ha_20 <- bccs %>% filter(DATEOFDEATH_YEAR >= 2020) %>% group_by(dateofdeath_floor, FINAL_DERIVED_HA, FNCF_MATCH) %>% summarise(total=n())
by_month_ha_20 %>% 
  filter(FNCF_MATCH == 1) %>% 
  filter(dateofdeath_floor %in% one_yr_roll) %>%
  ggplot(aes(x=dateofdeath_floor, y = total, fill=as.character(FINAL_DERIVED_HA))) + 
  #geom_line(size = 1.2) + 
  geom_bar(stat="identity", position = "stack") +
  standards + 
  scale_x_date(labels=format_dates, expand=c(0,0), date_breaks = "1 month")+
  scale_fill_manual(values = c("#F04B52", "#2C97A7", "#F57E20",  "#00A14B", "#8ACAE2"), 
                    labels = c("Interior", "Fraser", "Vancouver Coastal", "Vancouver Island", "Northern")
  ) + 
  labs(title = "Overdose Deaths by Month", 
       x="Month", y="Number of Overdose Deaths") + 
  theme(legend.position = "bottom", 
        axis.text.x=element_text(angle=45, hjust=1)) +
  geom_text(aes(label=total), position=position_stack(vjust=0.5), size = 5) 
#facet_grid(FINAL_DERIVED_HA ~., scales="free")
# ggsave(file = "deaths_by_region_month.jpg", path = "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/January" , width = 6, height = 6) 
# ggsave(file = "deaths_by_region_month.jpg", path = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/October" , width = 12.85, height = 6)
ggsave(here(reporting_mth, file="deaths_by_region_month.jpg"), width = 6, height = 6)


by_month_ha_20 %>% 
  filter(FNCF_MATCH == 1) %>% 
  filter(FINAL_DERIVED_HA == 1) %>%
  filter(dateofdeath_floor %in% one_yr_roll) %>%
  ggplot(aes(x=dateofdeath_floor, y = total, fill=as.character(FINAL_DERIVED_HA))) + 
  #geom_line(size = 1.2) + 
  geom_bar(stat="identity", position = "stack") +
  standards + 
  scale_x_date(labels=format_dates, expand=c(0,0), date_breaks = "1 month")+
  scale_fill_manual(values = c("#F04B52"), 
                    labels = c("Interior")
  ) + 
  coord_cartesian(ylim = c(0 , 17)) +
  labs(title = "Overdose Deaths by Month - Interior", 
       x="Month", y="Number of Overdose Deaths") + 
  theme(legend.position = "bottom", 
        axis.text.x=element_text(angle=45, hjust=1)) +
  geom_text(aes(label=total), position=position_stack(vjust=0.5), size = 5) 
#facet_grid(FINAL_DERIVED_HA ~., scales="free")
# ggsave(file = "deaths_by_region_month.jpg", path = "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/2021/September" , width = 12.85, height = 6) 
# ggsave(file = "deaths_by_region_month_INTERIOR.jpg", path = "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/January" , width = 6, height = 6)
ggsave(here(reporting_mth, file="deaths_by_region_month_INTERIOR.jpg"), width = 6, height = 6)


by_month_ha_20 %>% 
  filter(FNCF_MATCH == 1) %>% 
  filter(FINAL_DERIVED_HA == 2) %>%
  filter(dateofdeath_floor %in% one_yr_roll) %>%
  ggplot(aes(x=dateofdeath_floor, y = total, fill=as.character(FINAL_DERIVED_HA))) + 
  #geom_line(size = 1.2) + 
  geom_bar(stat="identity", position = "stack") +
  standards + 
  scale_x_date(labels=format_dates, expand=c(0,0), date_breaks = "1 month")+
  scale_fill_manual(values = c("#2C97A7"), 
                    labels = c("Fraser")
  ) +
  coord_cartesian(ylim = c(0 , 17)) +
  
  labs(title = "Overdose Deaths by Month - Fraser", 
       x="Month", y="Number of Overdose Deaths") + 
  theme(legend.position = "bottom", 
        axis.text.x=element_text(angle=45, hjust=1)) +
  geom_text(aes(label=total), position=position_stack(vjust=0.5), size = 5) 
#facet_grid(FINAL_DERIVED_HA ~., scales="free")
# ggsave(file = "deaths_by_region_month_FRASER.jpg", path = "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/January" , width = 6, height = 6)
# ggsave(file = "deaths_by_region_month_FRASER.jpg", path = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/October" , width = 6, height = 6)
ggsave(here(reporting_mth, file="deaths_by_region_month_FRASER.jpg"), width = 6, height = 6)

by_month_ha_20%>% 
  filter(FNCF_MATCH == 1) %>% 
  filter(FINAL_DERIVED_HA == 3) %>%
  filter(dateofdeath_floor %in% one_yr_roll) %>%
  ggplot(aes(x=dateofdeath_floor, y = total, fill=as.character(FINAL_DERIVED_HA))) + 
  #geom_line(size = 1.2) + 
  geom_bar(stat="identity", position = "stack") +
  standards + 
  scale_x_date(labels=format_dates, expand=c(0,0), date_breaks = "1 month")+
  scale_fill_manual(values = c("#F57E20"), 
                    labels = c("Vancouver Coastal")
  ) + 
  coord_cartesian(ylim = c(0 , 17)) +
  
  labs(title = "Overdose Deaths by Month - Vancouver Coastal", 
       x="Month", y="Number of Overdose Deaths") + 
  theme(legend.position = "bottom", 
        axis.text.x=element_text(angle=45, hjust=1)) +
  geom_text(aes(label=total), position=position_stack(vjust=0.5), size = 5) 
#facet_grid(FINAL_DERIVED_HA ~., scales="free")
# ggsave(file = "deaths_by_region_month_VC.jpg", path = "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/January", width = 6, height = 6 ) 
# ggsave(file = "deaths_by_region_month_VC.jpg", path = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/October" , width = 12.85, height = 6)
ggsave(here(reporting_mth, file="deaths_by_region_month_VC.jpg"), width = 6, height = 6)



by_month_ha_20 %>% 
  filter(FNCF_MATCH == 1) %>% 
  filter(FINAL_DERIVED_HA == 4) %>%
  filter(dateofdeath_floor %in% one_yr_roll) %>%
  ggplot(aes(x=dateofdeath_floor, y = total, fill=as.character(FINAL_DERIVED_HA))) + 
  #geom_line(size = 1.2) + 
  geom_bar(stat="identity", position = "stack") +
  standards + 
  scale_x_date(labels=format_dates, expand=c(0,0), date_breaks = "1 month")+
  scale_fill_manual(values = c("#00A14B"), 
                    labels = c( "Vancouver Island")
  ) + 
  coord_cartesian(ylim = c(0 , 17)) +
  labs(title = "Overdose Deaths by Month - Vancouver Island", 
       x="Month", y="Number of Overdose Deaths") + 
  theme(legend.position = "bottom", 
        axis.text.x=element_text(angle=45, hjust=1)) +
  geom_text(aes(label=total), position=position_stack(vjust=0.5), size = 5) 
#facet_grid(FINAL_DERIVED_HA ~., scales="free")
# ggsave(file = "deaths_by_region_month_VI.jpg", path = "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/January" , width = 6, height = 6) 
# ggsave(file = "deaths_by_region_month_VI.jpg", path = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/October" , width = 12.85, height = 6)
ggsave(here(reporting_mth, file="deaths_by_region_month_VI.jpg"), width = 6, height = 6)



by_month_ha_20%>% 
  filter(FNCF_MATCH == 1) %>% 
  filter(FINAL_DERIVED_HA == 5) %>%
  filter(dateofdeath_floor %in% one_yr_roll) %>%
  ggplot(aes(x=dateofdeath_floor, y = total, fill=as.character(FINAL_DERIVED_HA))) + 
  #geom_line(size = 1.2) + 
  geom_bar(stat="identity", position = "stack") +
  standards + 
  scale_x_date(labels=format_dates, expand=c(0,0), date_breaks = "1 month")+
  scale_fill_manual(values = c( "#8ACAE2"), 
                    labels = c( "Northern")
  ) + 
  coord_cartesian(ylim = c(0 , 17)) +
  labs(title = "Overdose Deaths by Month", 
       x="Month", y="Number of Overdose Deaths - Northern") + 
  theme(legend.position = "bottom", 
        axis.text.x=element_text(angle=45, hjust=1)) +
  geom_text(aes(label=total), position=position_stack(vjust=0.5), size = 5) 
#facet_grid(FINAL_DERIVED_HA ~., scales="free")
# ggsave(file = "deaths_by_region_month_Northern.jpg", path = "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/January" , width = 6, height = 6)
# ggsave(file = "deaths_by_region_month_Northern.jpg", path = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/October" , width = 12.85, height = 6)
ggsave(here(reporting_mth, file="deaths_by_region_month_Northern.jpg"), width = 6, height = 6)

by_month_ha_20_wide <- by_month_ha_20 %>% pivot_wider(names_from = dateofdeath_floor, values_from = total, values_fill = list(total=0)) %>% arrange(FINAL_DERIVED_HA, FNCF_MATCH)
write_csv(by_month_ha_20_wide, here(reporting_mth, "by_month_region.csv"))

by_month_sex_20 <- bccs %>% filter(DATEOFDEATH_YEAR >= 2020) %>% group_by(dateofdeath_floor, GENDER, FNCF_MATCH) %>% summarise(total=n())

by_month_sex_20 %>% 
  filter(FNCF_MATCH == 1) %>% 
  ggplot(aes(x=dateofdeath_floor, y = total, group=as.character(GENDER), colour=as.character(GENDER))) + 
  geom_line(size = 1.2) + 
  standards + 
  scale_x_date(labels = format_dates, expand=c(0,0), date_breaks = "1 month") + 
  scale_colour_manual(values = c("goldenrod3", "darkslateblue"), 
                    #  labels = c("Interior", "Fraser", "Vancouver Coastal", "Vancouver Island", "Northern")
  ) + 
  labs(title = "2021-2022 Overdose Deaths by Month", 
       x="Month", y="Number of Overdose Deaths") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle=45, hjust=1)) +
  geom_text_repel(aes(label=total), nudge_y = 0.5)#+ 
#  facet_grid(FNCF_MATCH ~., scales = "free")

ggsave(here(reporting_mth, file="deaths_by_sex_month.jpg"), width = 12.85, height = 6)



by_month_SEX_20_wide <- by_month_sex_20 %>% pivot_wider(names_from = month_lab, values_from = total, values_fill = list(total=0)) %>% arrange(GENDER, FNCF_MATCH)
write_csv(by_month_SEX_20_wide, here(reporting_mth, "by_month_sex.csv"))
