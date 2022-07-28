library(openxlsx)

"//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/2021/September"

can_std_pop <- read_csv(file = "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/r_readin_canadian_std_pop.CSV")
# can_std_pop <- read_csv(file = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/r_readin_canadian_std_pop.CSV")

#can_std_pop_sex <-read_csv(file="L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/rreadin_canadian_std_pop_sex.csv")

bc_fn_pop<- read_csv(file= "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/2020.11.27_fn_or_age_sex_pop.csv")
# bc_fn_pop<- read_csv(file= "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/2020.11.27_fn_or_age_sex_pop.csv")

###Update to most available month (ex if we have data up to April 2021, month_num =4)
month_num <- 9

bccs <- bccs %>% filter(DATEOFDEATH_YEAR > 2015) %>% 
  mutate(age_group_stds = ifelse(DECEASED_AGE < 10, "<10",
                                                          ifelse((DECEASED_AGE >= 10 & DECEASED_AGE <20), "10-19",
                                                                  ifelse((DECEASED_AGE >= 20 & DECEASED_AGE <30), "20-29", 
                                                                         ifelse((DECEASED_AGE >=30 & DECEASED_AGE <40), "30-39",
                                                                                ifelse((DECEASED_AGE >=40 & DECEASED_AGE <50), "40-49", 
                                                                                       ifelse((DECEASED_AGE >=50 & DECEASED_AGE <60), "50-59", 
                                                                                              ifelse((DECEASED_AGE >=60 & DECEASED_AGE <70), "60-69", 
                                                                                                     ifelse(DECEASED_AGE >=70, "70+", NA)))))))), 
                        FirstNations =ifelse(FNCF_MATCH == 1, "first_nations", "other_residents"))

## clean up std pop for bc level
std_pop_cl <- can_std_pop %>% dplyr::mutate(age_group_stds =  ifelse(age_group %in% "0 to 4 years"| age_group %in% "5 to 9 years", "<10", 
                                                              ifelse(age_group %in% "10 to 14 years"| age_group %in% "15 to 19 years", "10-19",
                                                                     ifelse(age_group %in% "20 to 24 years" | age_group %in% "25 to 29 years", "20-29", 
                                                                            ifelse(age_group %in% "30 to 34 years" | age_group %in% "35 to 39 years", "30-39", 
                                                                                   ifelse(age_group %in% "40 to 44 years" | age_group %in% "45 to 49 years", "40-49", 
                                                                                          ifelse(age_group %in% "50 to 54 years" | age_group %in% "55 to 59 years", "50-59",
                                                                                                 ifelse(age_group %in% "60 to 64 years" | age_group %in% "65 to 69 years", "60-69",
                                                                                                        #ifelse(age_group %in% "70 to 74 years" | age_group %in% "75 to 79 years", "70-79",
                                                                                                               "70+")))))))) %>% 
  dplyr::group_by(age_group_stds) %>%  
  dplyr::summarise(pop=sum(population, na.rm=T)) %>% 
  dplyr::arrange(age_group_stds)


## create template for all age groups 
all_ages <- tibble(age_group_stds = unique(by_age$age_group_stds), total_deaths = 0) %>% arrange(age_group_stds)

##create 2020 pop [same as 2019 pop] BC sex specific pop
twenty_pop <- bc_fn_pop %>% filter(year == 2019) %>% 
  mutate(year= 2020)
bc_fn_pop_21 <- bc_fn_pop %>% filter(year==2019) %>% mutate(year=2021)

bc_fn_pop_b <- bind_rows(bc_fn_pop, twenty_pop, bc_fn_pop_21)


## clean up pop file for sex 
##create 2020 pop [same as 2019 pop]

bc_fn_pop_sex <- bc_fn_pop_b %>% 
  dplyr::filter(!is.na(age_group_stds)) %>% 
  dplyr::arrange(year, fn_flag, gender, age_group_stds)# %>% 
#mutate(pop_adj = ifelse(year == 2021, month_num/12*Population, Population))

## create age and sex od deaths
by_age_sex <- bccs %>% 
  group_by(DATEOFDEATH_YEAR, age_group_stds, GENDER, FirstNations) %>% 
  summarise(total_deaths = n_distinct(MOH_STUDYID)) %>% 
  pivot_wider(names_from = FirstNations, 
              values_from = total_deaths, 
              values_fill = list(total_deaths = 0)) %>% 
  filter(!is.na(age_group_stds)) %>% 
  pivot_longer(names_to = "fn_flag",
               values_to = "total_deaths", 
               cols = first_nations:other_residents) %>% 
  arrange(DATEOFDEATH_YEAR, fn_flag, GENDER, age_group_stds) %>% 
  mutate(gender = ifelse(GENDER == "Male", "M", 
                         ifelse(GENDER == "Female", "F", "U")))

## loop for age adjustment
years = unique(by_age_sex$DATEOFDEATH_YEAR)
fn_l = unique(by_age_sex$fn_flag)
sex = unique(by_age_sex$gender)
sexb <- sex[sex != "U"]

combined <- tibble()

for (i in seq_along(fn_l)) {
  for (j in seq_along(years)) { 
   for (k in seq_along(sexb)) {
      
      subset <-  subset(by_age_sex, DATEOFDEATH_YEAR == years[j] & fn_flag == fn_l[i]
                      & gender == sexb[k]
                        ) 
      fn_pop_b <-  subset(bc_fn_pop_sex,  year == years[j] & fn_flag == fn_l[i] 
                      & gender == sexb[k]
                          )
      std_pop_b <- std_pop_cl
      
      if (nrow(subset) < nrow(all_ages)) {
        subset <- left_join(all_ages, subset, by="age_group_stds")
        subset$year <- years[j]
        subset$total_deaths = ifelse(is.na(subset$total_deaths.y) == T, 0, subset$total_deaths.y)
      }
      else {
        subset <- subset
      }
      
      adjust_year_i <- broom::tidy(epitools::ageadjust.direct(count = subset$total_deaths, pop=fn_pop_b$Population, stdpop = std_pop_b$pop)) %>% 
        mutate(year=years[j],
               fn_flag= fn_l[i], 
               sex = sexb[k]
               )
      
      combined <-bind_rows(combined, adjust_year_i)
      
      print(paste("Rates for:", fn_l[i], years[j]))
      print(adjust_year_i)
    }
 }
}

yearly_age_adj_wide_sex <- combined %>% 
  mutate(per_hundred_thousand_x = x*100000) %>% select(-x) %>% 
  pivot_wider(names_from = names, 
              values_from = per_hundred_thousand_x) %>% 
  arrange(year) %>% 
  mutate(adj_rate_proj = ifelse(year == 2021, adj.rate*12/month_num, adj.rate), 
         lci_proj = ifelse(year == 2021, lci*12/month_num, lci),
        uci_proj =ifelse(year == 2021, uci*12/month_num, uci), 
        crude_rate_proj = ifelse(year == 2021, crude.rate*12/month_num, crude.rate))

# write_csv(yearly_age_adj_wide_sex, file="age_adj_rates_yr.csv")
write_csv(yearly_age_adj_wide_sex, "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/September/age_adj_rates_yr.csv")



#######Rates by Sex 
ggplot(yearly_age_adj_wide_sex, aes(x=year, y=adj_rate_proj, group=sex, fill= sex)) + 
  geom_line(aes(color=sex), size = 1.2) +
  facet_wrap(fn_flag ~., labeller=labeller(fn_flag=sex_labels)) +
  scale_color_manual(values = c("lightgoldenrod1", "slateblue4"), name=NULL, labels = c("Female", "Male"))+
  standards + 
  geom_text(aes(label=round(adj_rate_proj, 1)), size=4.5, position = position_nudge(y=yearly_age_adj_wide_sex$label_pos)) +
  geom_ribbon(aes(ymin = lci_proj, ymax = uci_proj), alpha = 0.3, show.legend = F) +
  scale_fill_manual("",values=c("lightgoldenrod1", "slateblue4"), guide = F) +
  #geom_errorbar(aes(ymin = LLproj, ymax=ULproj), width = 0.2,color="blue") #position = position_dodge(0.05))+  
  #scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, by=30)) +
  standards +
  #theme_classic() + theme(axis.text.x = element_text(angle=90,vjust=0.1),plot.title = element_text(size = 11))+
  labs(title =paste("Figure 2B.Age Standardised Overdose Death Rates by Sex: First Nations & Other Residents"), 
       x = "Year",  y = "adjusted rate per 100,000")+theme(legend.position = "bottom") +
  theme(legend.position = "bottom", 
        strip.background = element_rect(fill="white"), 
        strip.text.x = element_text(
          size = 14, color = "black", face = "bold"))

# ggsave("age_std_rate_year_sex.jpeg", width = 9, height = 5)
ggsave("age_std_rate_year_sex.jpeg", path = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/September", width = 9, height = 5)
ggsave("age_std_rate_year_sex.jpeg")



# ggsave(file=here(folder_year, folder, "age_adjusted_HA_YTD_crude_mortality_rate.jpg"),  width = 7.16, height = 6.8)

#get total n of deaths and map back onto adjusted rates (NOT REQUIRED!!!)
by_sex_yr <- bccs %>% 
  group_by(DATEOFDEATH_YEAR, FirstNations, GENDER) %>% 
  summarise(total=n()) %>% 
  mutate(sex = ifelse(GENDER == "Male", "M", 
                         ifelse(GENDER == "Female", "F", "U"))) %>% 
  rename(year = "DATEOFDEATH_YEAR", fn_flag = "FirstNations")

yearly_age_adj_wide_sex_exp <- by_sex_yr %>% 
  left_join(yearly_age_adj_wide_sex, by=c("year", "fn_flag", "sex"))

write_csv(yearly_age_adj_wide_sex_exp, file =paste0("sex_specific_adjusted_rates_to", reporting_mth, folder_year, ".csv"))


adjusted_exp <- list(year_adj = yearly_age_adj_wide, year_sex_adj = yearly_age_adj_wide_sex, 
                     year_crude = export_crude_rates_proj, year_sex_crude = export_crude_rates_proj_sex)

openxlsx::write.xlsx(adjusted_exp, file = paste("adjusted_rates_ARI", Sys.Date(), ".xlsx", sep = "_"))

















