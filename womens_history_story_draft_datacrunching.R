
################# DATA PROCESSING ##############################################
#devtools::install_github('psrc/psrc.travelsurvey')
library(psrc.travelsurvey)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)
library(odbc)
library(DBI)
library(tidyr)
library(psrcplot)
library(srvyr)
library(survey)
library(tinytex)
library(psrccensus)
library(tidyverse)
library(knitr)
library(magick)
library(openxlsx)
library(htmlwidgets)
library(magrittr)





# Census and PUMS data about women, age, and employment##################################################

# pvars <- c("ESR","SEX", "AGEP", "BIN_AGE", "PRACE", "PINCP")
# hvars <- c("BINCOME", "HRACE")
# ftr_int <- function(x){as.integer(as.character(x))} 
# 
# pums19 <- get_psrc_pums(1, 2019, "p", pvars) 
# 
# pums19 %<>% mutate(
#   ESR= factor(
#     case_when(grepl("^(Civilian|Armed) ", as.character(ESR)) ~ "Employed",
#               !is.na(ESR) ~ "Not employed")),
#   AGE = factor(
#     case_when(between(ftr_int(AGEP), 18, 25) ~ '18-25',
#               between(ftr_int(AGEP), 26, 35) ~ '26-35',
#               between(ftr_int(AGEP), 36, 45) ~ '36-45',
#               AGEP >= 46 ~ "46+")),
#   PRACE= factor(
#     case_when(grepl("^(Indian|Asian|Black|Hispanic|Hawaiian|Other|Two)", as.character(PRACE)) ~ "POC",
#             !is.na(PRACE) ~ "White")))
# 
# #pums19 %>% 
#  # mutate(PRACE = factor(case_when(PRACE %in% c("American Indian or Alaskan Native Alone", "Asian alone", 
#   #                                    "Black or African American alone", 
#    #                                   "Hispanic or Latino", "Native Hawaiian and Other Pacific Islander alone",
#     #                                  "Some Other Race alone", "Two or More Races") ~ "POC",
#      #                    PRACE == "White alone" ~ "White")))
# 
# pums19_all <- psrc_pums_count(pums19, group_vars = c("ESR", "SEX"), incl_na=FALSE)%>%
#   filter(ESR != "Total")%>%
#   filter(SEX != "Total")%>%
#   rename(
#     survey = DATA_YEAR
#   )
# 
# pums19_race <- psrc_pums_count(pums19, group_vars = c("ESR", "PRACE", "SEX"), incl_na=FALSE)%>%
#   filter(ESR != "Total")%>%
#   filter(SEX != "Total")%>%
#   filter(PRACE != "Total")%>%
#   rename(
#     survey = DATA_YEAR
#   )
# 
# 
# pums21 <- get_psrc_pums(1, 2021, "p", pvars) 
# 
# pums21 %<>% mutate(
#   ESR= factor(
#     case_when(ESR %in% c(1,2,4,5) ~ "Employed",
#               !is.na(ESR) ~ "Not employed")),
#   AGE = factor(
#     case_when(between(ftr_int(AGEP), 18, 25) ~ '18-25',
#               between(ftr_int(AGEP), 26, 35) ~ '26-35',
#               between(ftr_int(AGEP), 36, 45) ~ '36-45',
#               AGEP >= 46 ~ "46+")))
# 
# pums21_all <- psrc_pums_count(pums21, group_vars = c("ESR", "SEX"),incl_na=FALSE)%>%
#   rename(
#     survey = DATA_YEAR
#   )
# 
# 
# 
# 
# pums19_sex_age <- psrc_pums_count(pums19, group_vars = c("SEX", "BIN_AGE"),incl_na=FALSE)%>%
#   filter(SEX != "Total")%>%
#   filter(BIN_AGE != "Total")%>%
#   filter(BIN_AGE == c("between 65 and 75 years", "between 75 and 85 years", "85 years and over"))%>%
#   rename(
#     survey = DATA_YEAR
#   )
# 
# 
# pums21_sex_age <- psrc_pums_count(pums21, group_vars = c("SEX", "BIN_AGE"),incl_na=FALSE)%>%
#   filter(SEX != "Total")%>%
#   filter(BIN_AGE != "Total")%>%
#   filter(BIN_AGE == c("between 65 and 75 years", "between 75 and 85 years", "85 years and over"))%>%
#   rename(
#     survey = DATA_YEAR
#   )








# DATA ANALYSIS
# read in hh survey trip data
db.connect <- function(adatabase) {
  elmer_connection <- dbConnect(odbc(),
                                driver = "SQL Server",
                                server = "AWS-PROD-SQL\\SOCKEYE",
                                database = adatabase,
                                trusted_connection = "yes"
  )
}

# read table
read.dt <- function(adatabase, atable) {
  elmer_connection <- db.connect(adatabase)
  dtelm <- dbReadTable(elmer_connection, SQL(atable))
  dbDisconnect(elmer_connection)
  return(dtelm)
}

# read-in variable metadata table for levels
vars_meta <- read.dt('Elmer', 'HHSurvey.variable_metadata')


mode_vars<-c('mode_1', 'mode_simple', 'travelers_total')
other_vars<-c('final_home_rgcnum', 'hhsize', 'vehicle_count',  "hhincome_broad", 'rent_own', 'res_dur', 'student', 'education',  'hhincome_detailed', 
              "age", "age_category", 'race_category', 'race_eth_broad', 'gender', 'employment',  'lifecycle', 'mode_acc', 'dest_purpose_cat', 'origin_purpose_cat', 'final_home_is_rgc',
              'race_eth_poc_update')
trip_path_dist<-'trip_path_distance'
all_vars<-c(mode_vars, other_vars, trip_path_dist)

data_17_19<- get_hhts("2017_2019", "t", vars=all_vars)%>% mutate(year=ifelse(survey=='2017_2019', '2017/2019', '2021'))

data_21<- get_hhts("2021", "t", vars=all_vars)%>% mutate(year=ifelse(survey=='2017_2019', '2017/2019', '2021'))




# set up trip data for analysis
data_17_19<-data_17_19%>%mutate(NoVehicles=ifelse(vehicle_count=='0 (no vehicles)', 'No Vehicles', "Has Vehicles"))%>%
  mutate(hhsize_simple=case_when(hhsize== '4 people' ~'4 or more people',                                                                     hhsize== '5 people' ~'4 or more people',
                                 hhsize== '6 people' ~'4 or more people',
                                 hhsize== '7 people' ~'4 or more people',
                                 hhsize== '8 people' ~'4 or more people',
                                 hhsize== '12 people' ~'4 or more people',
                                 TRUE ~ hhsize))%>%
  mutate(hhincome_100= case_when(hhincome_broad=='$100,000-$199,000' ~ '$100,000 or more',
                                 hhincome_broad=='$200,000 or more' ~ '$100,000 or more',
                                 TRUE~hhincome_broad))%>%
  mutate(edu_simple= case_when(education=='Bachelor degree' ~ 'Bachelors or higher', 
                               education=='Graduate/Post-graduate degree' ~ 'Bachelors or higher',
                               TRUE ~ 'Less than Bachelors degree'))%>%
  mutate(age_grp= case_when(age=='75-84 years' ~ '75 years or older', 
                            age == '85 or years older' ~ '75 years or older',
                            TRUE ~ age))%>%
  mutate(gender_grp= case_when(gender == 'Prefer not to answer' ~ 'Non-binary, another, prefer not to answer',
                               gender=='Not listed here / prefer not to answer' ~ 'Non-binary, another, prefer not to answer',
                               gender=='Non-Binary'~ 'Non-binary, another, prefer not to answer',
                               gender=='Another'~ 'Non-binary, another, prefer not to answer',
                               TRUE ~ gender))%>%mutate(work_purpose=ifelse(dest_purpose_cat=='Work', 'Work', 'Not Work'))%>%
  
  mutate(race_short= str_extract(race_eth_broad,  "^[^ ]+"))%>%
  mutate(simple_purpose=ifelse(dest_purpose_cat=='Home', origin_purpose_cat, dest_purpose_cat))%>%
  mutate(simple_purpose=case_when(simple_purpose=='Work'~ 'Work School',
                                  simple_purpose=='School'~ 'Work School',
                                  simple_purpose=='Work-related'~ 'Work School',
                                  simple_purpose=='Shop'~ 'Shop',
                                  simple_purpose=='Escort'~ 'Escort Passenger',
                                  simple_purpose=='Errand/Other'~ 'Errands Other',
                                  simple_purpose=='Change mode'~ 'Errands Other',
                                  simple_purpose=='Social/Recreation' ~ 'Social, Recreation, Meal',
                                  simple_purpose=='Meal' ~ 'Social, Recreation, Meal',
                                  simple_purpose=='Home' ~'Errands Other',
                                  is.na(simple_purpose) ~ 'Errands Other',
                                  TRUE ~ simple_purpose))%>%mutate(rgc=as.factor(final_home_is_rgc))%>%
  mutate(non_motorized_mode=ifelse((mode_simple=='Walk'|mode_simple=='Bike'),'Walk/Bike', 'Not Walk/Bike'))%>%
  mutate(mode_acc_walk=ifelse(mode_acc=='Walked or jogged', 'Walked or jogged', 'Other Access Mode'))%>%
  mutate(travelers_total_grp= case_when(travelers_total<=1 ~ '1',
                                        travelers_total==2 ~ '2',
                                        travelers_total==3 ~ '3+',
                                        travelers_total==4 ~ '4+'))


data_21<-data_21%>%mutate(NoVehicles=ifelse(vehicle_count=='0 (no vehicles)', 'No Vehicles', "Has Vehicles"))%>%
  mutate(hhsize_simple=case_when(hhsize== '4 people' ~'4 or more people',                                                                    hhsize== '5 people' ~'4 or more people',
                                 hhsize== '6 people' ~'4 or more people',
                                 hhsize== '7 people' ~'4 or more people',
                                 hhsize== '8 people' ~'4 or more people',
                                 hhsize== '12 people' ~'4 or more people',
                                 TRUE ~ hhsize)) %>%
  mutate(hhincome_100= case_when(hhincome_broad=='$100,000-$199,000' ~ '$100,000 or more',
                                 hhincome_broad=='$200,000 or more' ~ '$100,000 or more',
                                 TRUE~hhincome_broad))%>%
  mutate(edu_simple= case_when(education=='Bachelor degree' ~ 'Bachelors or higher', 
                               education=='Graduate/Post-graduate degree' ~ 'Bachelors or higher',
                               TRUE ~ 'Less than Bachelors degree'))%>%
  mutate(age_grp= case_when(age=='75-84 years' ~ '75 years or older', 
                            age == '85 or years older' ~ '75 years or older',
                            TRUE ~ age))%>%
  mutate(gender_grp= case_when(gender == 'Prefer not to answer' ~ 'Non-binary, another, prefer not to answer',
                               gender=='Not listed here / prefer not to answer' ~ 'Non-binary, another, prefer not to answer',
                               gender=='Non-Binary'~ 'Non-binary, another, prefer not to answer', 
                               gender=='Another'~ 'Non-binary, another, prefer not to answer',
                               TRUE ~ gender))%>%
  mutate(work_purpose=ifelse(dest_purpose_cat=='Work', 'Work', 'Not Work'))%>%
  mutate(simple_purpose=ifelse(dest_purpose_cat=='Home', origin_purpose_cat, dest_purpose_cat))%>%
  mutate(simple_purpose=case_when(simple_purpose=='Work'~ 'Work School',
                                  simple_purpose=='School'~ 'Work School',
                                  simple_purpose=='Work-related'~ 'Work School',
                                  simple_purpose=='Shop'~ 'Shop',
                                  simple_purpose=='Escort'~ 'Escort Passenger',
                                  simple_purpose=='Errand/Other'~ 'Errands Other',
                                  simple_purpose=='Change mode'~ 'Errands Other',
                                  simple_purpose=='Social/Recreation' ~ 'Social, Recreation, Meal',
                                  simple_purpose=='Meal' ~ 'Social, Recreation, Meal',
                                  simple_purpose=='Home' ~'Errands Other',
                                  is.na(simple_purpose) ~ 'Errands Other',
                                  TRUE ~ simple_purpose))%>%mutate(rgc=as.factor(final_home_is_rgc))%>%
  mutate(non_motorized_mode=ifelse((mode_simple=='Walk'|mode_simple=='Bike'),'Walk/Bike', 'Not Walk/Bike'))%>%
  mutate(mode_acc_walk=ifelse(mode_acc=='Walked or jogged', 'Walked or jogged', 'Other Access Mode'))%>%
  mutate(travelers_total_grp= case_when(travelers_total<=1 ~ '1',
                                        travelers_total==2 ~ '2',
                                        travelers_total==3 ~ '3+',
                                        travelers_total==4 ~ '4+'))

data_17_19$hhincome_100_f=factor(data_17_19$hhincome_100, levels=c("Prefer not to answer",  "$100,000 or more","$75,000-$99,999", "$50,000-$74,999" ,"$25,000-$49,999" , "Under $25,000"  ))

data_21$hhincome_100_f=factor(data_21$hhincome_100, levels=c("Prefer not to answer",  "$100,000 or more","$75,000-$99,999", "$50,000-$74,999" ,"$25,000-$49,999" , "Under $25,000"  ))





# Trips by Purpose
# Shows women do more caring trips; men more work and school; 2017/2019 has a stronger correlation with gender than 2021. COVID conditions?

summs_2017_2019 <- hhts_count(data_17_19,
                              group_vars=c('gender','simple_purpose'),
                              spec_wgt='trip_adult_weight_2017_2019')%>%drop_na(c('gender', 'simple_purpose'))%>%filter(simple_purpose!='Total')%>%filter(gender %in% c('Female', 'Male') )

summs_2021 <- hhts_count(data_21,
                         group_vars=c('gender','simple_purpose'),
                         spec_wgt='trip_adult_weight_2021') %>%drop_na(c('gender', 'simple_purpose'))%>%filter(simple_purpose!='Total')%>%filter(gender %in% c('Female', 'Male') )



#static_column_chart(t= summs_2017_2019, x='simple_purpose', y='share',  fill='gender', moe='share_moe', est='percent')
#static_column_chart(t= summs_2021, x='simple_purpose', y='share',  fill='gender', moe='share_moe', est='percent')


summs_2017_2019_2021<-rbind(summs_2017_2019, summs_2021)%>%
  mutate(survey=case_when(survey == "2017_2019" ~ '2017/2019',
                   TRUE ~ '2021'))






summs_2017_2019_dist <- hhts_median(data_17_19,stat_var='trip_path_distance',
                                    group_vars=c('gender'),
                                    spec_wgt='trip_adult_weight_2017_2019')%>%drop_na(c('gender'))%>%filter(gender %in% c('Female', 'Male') )

summs_2021_dist <- hhts_median(data_21,stat_var='trip_path_distance',
                               group_vars=c('gender'),
                               spec_wgt='trip_adult_weight_2021') %>%drop_na(c('gender'))%>%filter(gender %in% c('Female', 'Male') )


static_column_chart(t= summs_2017_2019_dist, x='gender', y='trip_path_distance_median',  fill='gender', moe='trip_path_distance_median_moe', est='number')
#static_column_chart(t= summs_2021_dist, x='gender', y='trip_path_distance_median',  fill='gender', moe='trip_path_distance_median_moe', est='number')


#summs_2017_2019_2021<-rbind(summs_2017_2019_dist, summs_2021_dist)





# Trips by Number of Travelers

# Women tend to travel with more people

data_17_19_big_hh<- data_17_19%>%
  filter(!hhsize %in% c('1 person', '2 people'))
data_21_big_hh<-data_21%>%
  filter(!hhsize %in% c('1 person', '2 people'))

trav_summs_2017_2019 <- hhts_count(data_17_19_big_hh,
                                   group_vars=c('gender','travelers_total_grp'),
                                   spec_wgt='trip_adult_weight_2017_2019')%>%drop_na(c('gender', 'travelers_total_grp'))%>%filter(travelers_total_grp!='Total')%>%filter(travelers_total_grp!='1')%>%filter(gender %in% c('Female', 'Male') )

trav_summs_2021 <- hhts_count(data_21_big_hh,
                              group_vars=c('gender','travelers_total_grp'),
                              spec_wgt='trip_adult_weight_2021') %>%drop_na(c('gender', 'travelers_total_grp'))%>%filter(travelers_total_grp!='Total')%>%filter(travelers_total_grp!='1')%>%filter(gender %in% c('Female', 'Male') )







mode_summs_2017_2019 <- hhts_count(data_17_19,
                                   group_vars=c('gender_grp','mode_simple'),
                                   spec_wgt='trip_adult_weight_2017_2019')%>%
  drop_na(c('gender_grp', 'mode_simple'))%>%
  filter(mode_simple=='Bike')%>%
 # filter(gender %in% c('Female', 'Male') )%>% 
  mutate_if(is.numeric, round, -3)

mode_summs_2021 <- hhts_count(data_21,
                              group_vars=c('gender_grp','mode_simple'),
                              spec_wgt='trip_adult_weight_2021') %>%
  drop_na(c('gender_grp', 'mode_simple'))%>%
  filter(mode_simple=='Bike')%>%
  #filter(gender %in% c('Female', 'Male') )%>% 
  mutate_if(is.numeric, round, -3)








# GET
get_telecommute_data <- function(survey, stat_var, group_vars, weight, incl_na = TRUE) {
  
  if(survey == "2021") {
    sdf <- get_hhts(survey = survey,
                    level = "p",
                    vars = c("age_category",
                             "worker",
                             "workplace",
                             "gender",
                             'race_eth_poc_update',
                             "race_category",
                             "telecommute_freq",
                             "benefits_1",
                             "benefits_2",
                             "benefits_3",
                             "industry")) %>% 
      filter(age_category != "Under 18 years"
             & worker != "No jobs") %>% 
      mutate(telecommute_freq_cond = case_when(telecommute_freq %in% c("1-2 days", "3-4 days", "5+ days")
                                               ~ "1+ days per week",
                                               !is.na(telecommute_freq) ~ telecommute_freq),
             workplace_travel = case_when(workplace %in% c("Usually the same location (outside home)",
                                                           "Workplace regularly varies (different offices or jobsites)",
                                                           "Drives for a living (e.g., bus driver, salesperson)")
                                          ~ "Works outside the home",
                                          workplace %in% c("Telework some days and travel to a work location some days",
                                                           "At home (telecommute or self-employed with home office)")
                                          ~ "Works at home",
                                          !is.na(workplace) ~ workplace),
             gender_group = case_when(gender %in% c("Not listed here / prefer not to answer", "Non-Binary")
                                      ~ "Prefer not to answer / Another",
                                      !is.na(gender) ~ gender),
             race_group = case_when(race_category %in% c("African American", "Asian", "Hispanic", "Other") ~ "POC",
                                    race_category == "White Only" ~ "White",
                                    !is.na(race_category) ~ race_category),
             industry = str_trim(industry)) %>% 
      mutate(industry_cond = case_when(
        industry %in% c("Construction", "Natural resources (e.g., forestry, fishery, energy)")
        ~ "Construction & Resources",
        industry == "Personal services (e.g., hair styling, personal assistance, pet sitting)"
        ~ "Personal Services",
        industry == "Manufacturing (e.g., aerospace & defense, electrical, machinery)"
        ~ "Manufacturing",
        industry %in% c("Financial services", "Real estate")
        ~ "Finance & Real Estate",
        industry %in% c("Public education", "Private education")
        ~ "Education (all)",
        industry %in% c("Health care", "Social assistance", "Childcare (e.g., nanny, babysitter)")
        ~ "Health Care, Social Services, & Childcare",
        industry %in% c("Arts and entertainment", "Media")
        ~ "Media & Entertainment",
        industry %in% c("Hospitality (e.g., restaurant, accommodation)", "Retail")
        ~ "Hospitality & Retail",
        industry %in% c("Landscaping", "Sports and fitness", "Other")
        ~ "Other",
        industry == "Government"
        ~ "Government",
        industry == "Military"
        ~ "Military",
        industry == "Missing: Skip Logic"
        ~ "Missing",
        industry == "Professional and business services (e.g., consulting, legal, marketing)"
        ~ "Professional & Business Services",
        industry == "Technology and telecommunications"
        ~ "Technology & Telecommunications",
        industry == "Transportation and utilities"
        ~ "Transportation & Utilities"))
  } else {
    sdf <- get_hhts(survey = survey,
                    level = "p",
                    vars = c("age_category",
                             "worker",
                             "employment",
                             "workplace",
                             "race_eth_poc_update",
                             "gender",
                             "race_category",
                             "telecommute_freq",
                             "benefits_1",
                             "benefits_2",
                             "benefits_3")) %>% 
      filter(age_category != "Under 18 years"
             & worker != "No jobs") %>% 
      mutate(telecommute_freq = case_when(telecommute_freq %in% c("1 day a week", "2 days a week") ~ "1-2 days", 
                                          telecommute_freq %in% c("3 days a week", "4 days a week") ~ "3-4 days", 
                                          telecommute_freq %in% c("5 days a week", "6-7 days a week") ~ "5+ days",
                                          telecommute_freq %in% c("Never", "Not applicable") ~ "Never / None",
                                          !is.na(telecommute_freq) ~ telecommute_freq)) %>% 
      mutate(telecommute_freq_cond = case_when(telecommute_freq %in% c("1-2 days", "3-4 days", "5+ days")
                                               ~ "1+ days per week",
                                               !is.na(telecommute_freq) ~ telecommute_freq),
             workplace_travel = case_when(workplace %in% c("Usually the same location (outside home)",
                                                           "Workplace regularly varies (different offices or jobsites)",
                                                           "Drives for a living (e.g., bus driver, salesperson)")
                                          ~ "Works outside the home",
                                          workplace == "At home (telecommute or self-employed with home office)"
                                          ~ "Works at home",
                                          !is.na(workplace) ~ "Missing"),
             gender_group = case_when(gender %in% c("Prefer not to answer", "Another")
                                      ~ "Prefer not to answer / Another",
                                      !is.na(gender) ~ gender),
             race_group = case_when(race_category %in% c("African American", "Asian", "Hispanic", "Other") ~ "POC",
                                    race_category == "White Only" ~ "White",
                                    !is.na(race_category) ~ race_category))
  }
  
  sdf$survey <- recode(sdf$survey, `2017_2019` = "2017/2019", .default = levels(sdf$survey))
  sdf$race_category <- recode(sdf$race_category, `White Only` = "White", .default = levels(sdf$race_category))
  
  stats <- hhts_count(df = sdf,
                      stat_var = stat_var,
                      group_vars = group_vars,
                      spec_wgt = weight,
                      incl_na = incl_na)
  
  return(stats)
}


telecommute_data_organize <- function(survey, stat_var, group_vars, weight, incl_na = TRUE) {
  
  if(survey == "2021") {
    sdf <- get_hhts(survey = survey,
                    level = "p",
                    vars = c("age_category",
                             "worker",
                             "workplace",
                             "gender",
                             'race_eth_poc_update',
                             "race_category",
                             "telecommute_freq",
                             "benefits_1",
                             "benefits_2",
                             "benefits_3",
                             "industry")) %>% 
      filter(age_category != "Under 18 years"
             & worker != "No jobs") %>% 
      mutate(telecommute_freq_cond = case_when(telecommute_freq %in% c("1-2 days", "3-4 days", "5+ days")
                                               ~ "1+ days per week",
                                               !is.na(telecommute_freq) ~ telecommute_freq),
             workplace_travel = case_when(workplace %in% c("Usually the same location (outside home)",
                                                           "Workplace regularly varies (different offices or jobsites)",
                                                           "Drives for a living (e.g., bus driver, salesperson)")
                                          ~ "Works outside the home",
                                          workplace %in% c("Telework some days and travel to a work location some days",
                                                           "At home (telecommute or self-employed with home office)")
                                          ~ "Works at home",
                                          !is.na(workplace) ~ workplace),
             gender_group = case_when(gender %in% c("Not listed here / prefer not to answer", "Non-Binary")
                                      ~ "Prefer not to answer / Another",
                                      !is.na(gender) ~ gender),
             race_group = case_when(race_category %in% c("African American", "Asian", "Hispanic", "Other") ~ "POC",
                                    race_category == "White Only" ~ "White",
                                    !is.na(race_category) ~ race_category),
             industry = str_trim(industry)) %>% 
      mutate(industry_cond = case_when(
        industry %in% c("Construction", "Natural resources (e.g., forestry, fishery, energy)")
        ~ "Construction & Resources",
        industry == "Personal services (e.g., hair styling, personal assistance, pet sitting)"
        ~ "Personal Services",
        industry == "Manufacturing (e.g., aerospace & defense, electrical, machinery)"
        ~ "Manufacturing",
        industry %in% c("Financial services", "Real estate")
        ~ "Finance & Real Estate",
        industry %in% c("Public education", "Private education")
        ~ "Education (all)",
        industry %in% c("Health care", "Social assistance", "Childcare (e.g., nanny, babysitter)")
        ~ "Health Care, Social Services, & Childcare",
        industry %in% c("Arts and entertainment", "Media")
        ~ "Media & Entertainment",
        industry %in% c("Hospitality (e.g., restaurant, accommodation)", "Retail")
        ~ "Hospitality & Retail",
        industry %in% c("Landscaping", "Sports and fitness", "Other")
        ~ "Other",
        industry == "Government"
        ~ "Government",
        industry == "Military"
        ~ "Military",
        industry == "Missing: Skip Logic"
        ~ "Missing",
        industry == "Professional and business services (e.g., consulting, legal, marketing)"
        ~ "Professional & Business Services",
        industry == "Technology and telecommunications"
        ~ "Technology & Telecommunications",
        industry == "Transportation and utilities"
        ~ "Transportation & Utilities"))
  } else {
    sdf <- get_hhts(survey = survey,
                    level = "p",
                    vars = c("age_category",
                             "worker",
                             "employment",
                             "workplace",
                             "race_eth_poc_update",
                             "gender",
                             "race_category",
                             "telecommute_freq",
                             "benefits_1",
                             "benefits_2",
                             "benefits_3")) %>% 
      filter(age_category != "Under 18 years"
             & worker != "No jobs") %>% 
      mutate(telecommute_freq = case_when(telecommute_freq %in% c("1 day a week", "2 days a week") ~ "1-2 days", 
                                          telecommute_freq %in% c("3 days a week", "4 days a week") ~ "3-4 days", 
                                          telecommute_freq %in% c("5 days a week", "6-7 days a week") ~ "5+ days",
                                          telecommute_freq %in% c("Never", "Not applicable") ~ "Never / None",
                                          !is.na(telecommute_freq) ~ telecommute_freq)) %>% 
      mutate(telecommute_freq_cond = case_when(telecommute_freq %in% c("1-2 days", "3-4 days", "5+ days")
                                               ~ "1+ days per week",
                                               !is.na(telecommute_freq) ~ telecommute_freq),
             workplace_travel = case_when(workplace %in% c("Usually the same location (outside home)",
                                                           "Workplace regularly varies (different offices or jobsites)",
                                                           "Drives for a living (e.g., bus driver, salesperson)")
                                          ~ "Works outside the home",
                                          workplace == "At home (telecommute or self-employed with home office)"
                                          ~ "Works at home",
                                          !is.na(workplace) ~ "Missing"),
             gender_group = case_when(gender %in% c("Prefer not to answer", "Another")
                                      ~ "Prefer not to answer / Another",
                                      !is.na(gender) ~ gender),
             race_group = case_when(race_category %in% c("African American", "Asian", "Hispanic", "Other") ~ "POC",
                                    race_category == "White Only" ~ "White",
                                    !is.na(race_category) ~ race_category))
  }
  
return(sdf)
}


# Workplace
travel_by_gender_17_19_raw <- telecommute_data_organize(survey = "2017_2019",
                                               stat_var = "workplace_travel",
                                               group_vars = c("gender_group", "race_eth_poc_update', 'workplace_travel"),
                                               weight = "hh_weight_2017_2019_adult",
                                               incl_na = FALSE) 

travel_by_gender_17_19<-hhts_count(travel_by_gender_17_19_raw, group_vars=c('race_eth_poc_update', 'gender','workplace_travel'))%>% 
  filter(gender %in% c('Male', 'Female'))%>%filter(workplace_travel=='Works at home')%>%filter(race_eth_poc_update !='No response')
#%>%filter(workplace_travel=='Works at home')%>%
 # filter(gender_group %in% c('Female', 'Male'))

travel_by_gender_21_raw <- telecommute_data_organize(survey = "2021",
                                            stat_var = "workplace_travel",
                                            group_vars = c("gender_group", "race_eth_poc_update", "workplace_travel"),
                                            weight = "person_adult_weight_2021",
                                            incl_na = FALSE)
travel_by_gender_21<-hhts_count(travel_by_gender_21_raw, group_vars=c('race_eth_poc_update', 'gender','workplace_travel'))%>% 
  filter(gender %in% c('Male', 'Female'))%>%filter(workplace_travel=='Works at home')%>%filter(race_eth_poc_update !='No response')
work_loc_trend<-rbind(travel_by_gender_17_19, travel_by_gender_21)%>% mutate(year=ifelse(survey=='2017_2019', '2017/2019', '2021'))

## updates for income for race and gender (PUMS) from 2021 to include for 2023

#inc_sex_srvyr_obj<-get_psrc_pums(span=5, dyear=2021, level="p", vars=c("SEX","PINCP","WKHP", 'PRACE'))%>% filter(WKHP>30)
#inc_sex_2021<-psrc_pums_median(inc_sex_srvyr_obj, stat_var='PINCP',group_vars = c("SEX",'PRACE'))%>%filter(COUNTY=='Region')



#pums19 %>% 
# mutate(PRACE = factor(case_when(PRACE %in% c("American Indian or Alaskan Native Alone"1, "Asian alone", 
#                                    "Black or African American alone", 
#                                   "Hispanic or Latino", "Native Hawaiian and Other Pacific Islander alone",
#                                  "Some Other Race alone", "Two or More Races") ~ "POC",
#                    PRACE == "White alone" ~ "White"))))

trips_by_mode_17_19_transit<-hhts_count(data_17_19, group_vars=c('race_eth_poc_update', 'gender','mode_simple'))%>%
  filter(mode_simple!='Total')%>%filter(gender=='Male' | gender=='Female')%>%filter(race_eth_poc_update!='Total')%>%
  filter(mode_simple=='Transit')%>%filter(race_eth_poc_update !='No response')
trips_by_mode_21_transit<-hhts_count(data_21, group_vars=c( 'race_eth_poc_update','gender','mode_simple'))%>%
  filter(mode_simple!='Total')%>%filter(gender=='Male' | gender=='Female')%>%filter(race_eth_poc_update!='Total')%>%
  filter(mode_simple=='Transit')%>%filter(race_eth_poc_update !='No response')


trips_by_mode_transit<-rbind(trips_by_mode_17_19_transit, trips_by_mode_21_transit)%>% mutate(year=ifelse(survey=='2017_2019', '2017/2019', '2021'))
