library(openxlsx)

can_std_pop <- read_csv(file = "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/r_readin_canadian_std_pop.CSV")
# can_std_pop <- read_csv(file = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/r_readin_canadian_std_pop.CSV")
# bc_fn_pop<- read_csv(file= "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/2020.11.27_fn_or_age_sex_pop.csv")
bc_fn_pop<- read_csv(file= "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/2020.11.27_fn_or_age_sex_pop.csv")

month_num <- 4

bccs <- bccs %>% filter(DATEOFDEATH_YEAR > 2015) %>% 
  # mutate(DATEOFDEATH = DATE_OF_DEATH) %>%
  mutate(age_group_stds = ifelse(DECEASED_AGE < 10, "<10",
                                                          ifelse((DECEASED_AGE >= 10 & DECEASED_AGE <20), "10-19",
                                                                  ifelse((DECEASED_AGE >= 20 & DECEASED_AGE <30), "20-29", 
                                                                         ifelse((DECEASED_AGE >=30 & DECEASED_AGE <40), "30-39",
                                                                                ifelse((DECEASED_AGE >=40 & DECEASED_AGE <50), "40-49", 
                                                                                       ifelse((DECEASED_AGE >=50 & DECEASED_AGE <60), "50-59", 
                                                                                              ifelse((DECEASED_AGE >=60 & DECEASED_AGE <70), "60-69", 
                                                                                                     ifelse(DECEASED_AGE >=70, "70+", NA)))))))), 
                        FirstNations =ifelse(FNCF_MATCH == 1, "first_nations", "other_residents"))
#####group by age od deaths
by_age <- bccs %>% 
  dplyr::group_by(DATEOFDEATH_YEAR, age_group_stds, FirstNations) %>% 
  dplyr::summarise(total_deaths = n_distinct(MOH_STUDYID)) %>% 
  pivot_wider(names_from = FirstNations, 
              values_from = total_deaths, 
              values_fill = list(total_deaths = 0)) %>% 
  dplyr::filter(!is.na(age_group_stds)) %>% 
  pivot_longer(names_to = "fn_flag",
               values_to = "total_deaths", 
               cols = first_nations:other_residents) %>% 
  dplyr::arrange(DATEOFDEATH_YEAR, fn_flag, age_group_stds)

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

##clean up pop file full pop
bc_fn_pop_t <- bc_fn_pop_b %>% 
  dplyr::group_by(year, fn_flag, age_group_stds) %>% 
  dplyr::summarise(Population = sum(Population, na.rm = T)) %>% 
  dplyr::filter(!is.na(age_group_stds)) %>% 
  dplyr::arrange(year, fn_flag, age_group_stds)


## loop for age adjustment
years = unique(by_age$DATEOFDEATH_YEAR)
fn_l = unique(by_age$fn_flag)



combined <- tibble()

for (i in seq_along(fn_l)) {
  for (j in seq_along(years)) { 
   # for (k in seq_along(sexb)) {
      
      subset <-  subset(by_age, DATEOFDEATH_YEAR == years[j] & fn_flag == fn_l[i]
                #       & gender == sexb[k]
                        ) 
      fn_pop_b <-  subset(bc_fn_pop_t,  year == years[j] & fn_flag == fn_l[i] 
                 #         & gender == sexb[k]
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
               #sex = sexb[k]
               )
      
      combined <-bind_rows(combined, adjust_year_i)
      
      print(paste("Rates for:", fn_l[i], years[j]))
      print(adjust_year_i)
    }
 }
#}

yearly_age_adj_wide_sex <- combined %>% 
  mutate(per_hundred_thousand_x = x*100000) %>% select(-x) %>% 
  pivot_wider(names_from = names, 
              values_from = per_hundred_thousand_x) %>% 
  arrange(year) %>% 
  mutate(adj_rate_proj = ifelse(year == 2021, adj.rate*12/month_num, adj.rate), 
         lci_proj = ifelse(year == 2021, lci*12/month_num, lci),
        uci_proj =ifelse(year == 2021, uci*12/month_num, uci), 
        crude_rate_proj = ifelse(year == 2021, crude.rate*12/month_num, crude.rate))

# write_csv(yearly_age_adj_wide_sex, file=here(folder, "age_adj_rates_yr.csv"))
write_csv(yearly_age_adj_wide_sex, "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/September/age_adj_rates_yr.csv")


###########Rates by Year
ggplot(yearly_age_adj_wide_sex, aes(x=year, y=adj_rate_proj, group=fn_flag, fill= fn_flag)) + 
  geom_line(aes(color=fn_flag), size = 1.2) +
  scale_color_manual(values = c("darkorange1", "skyblue4"), name=NULL, labels = c("First Nations", "Other Residents")) +
  #geom_point(aes(color=FNCF_MATCH), size=0.07)+
  geom_text_repel(aes(label=round(adj_rate_proj, 1)), size=4.5)+ 
  geom_ribbon(aes(ymin = lci_proj, ymax = uci_proj), alpha = 0.3, show.legend = F) +
  scale_fill_manual("",values=c("darkorange1", "skyblue4"), guide = F) +
  #geom_errorbar(aes(ymin = LLproj, ymax=ULproj), width = 0.2,color="blue") #position = position_dodge(0.05))+  
  #scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, by=30)) +
  standards +
  #theme_classic() + theme(axis.text.x = element_text(angle=90,vjust=0.1),plot.title = element_text(size = 11))+
  labs(title =paste("Age Standardised Drug Poisoning Death Rates: First Nations & Other Residents"), 
       x = "Year",  y = "age-standardized rate per 100,000")+theme(legend.position = "bottom")

# ggsave(file=here(folder,"age_std_rate_year.jpeg"), width = 8, height = 5)
ggsave("age_std_rate_year.jpeg", path = "L:/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/SORT/sort_analyst_files/November-December", width = 8, height = 5)
# ggsave("age_std_rate_year.jpeg", path = "//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/2021/September", width = 8, height = 5)





sex_labels <- c(first_nations = "First Nations", other_residents = "Other Residents")

















