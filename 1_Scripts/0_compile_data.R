# source data
source("global_options.R")

#### Clean study identifying information ####
study <- read_delim(here("0_Data", "AACT", "studies.txt"),
                    delim = "|", escape_double = FALSE, trim_ws = TRUE)

#### Clean study brief description ####
summary <- read_delim(here("0_Data", "AACT", "brief_summaries.txt"),
                    delim = "|", escape_double = FALSE, trim_ws = TRUE) %>%
  rename(summary = "description")

#### Clean study locations ####
countries <- read_delim(here("0_Data", "AACT", "countries.txt"),
                        delim = "|", escape_double = FALSE, trim_ws = TRUE) 

#### Clean study keywords ####
keywords <- read_delim(here("0_Data", "AACT", "keywords.txt"),
                       delim = "|", escape_double = FALSE, trim_ws = TRUE) 

#### Clean study sponsors ####
sponsors <- read_delim(here("0_Data", "AACT", "sponsors.txt"),
                       delim = "|", escape_double = FALSE, trim_ws = TRUE) 

#### Clean randomization and research design ####
randomized <- read_delim(here("0_Data", "AACT", "designs.txt"),
                         delim = "|", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(randomized = ifelse(allocation == "Randomized",1,0))

table(randomized$randomized, randomized$allocation, useNA = "always") # what is up with the NA?

#### Clean research design groups ####
groups <- read_delim(here("0_Data", "AACT", "design_groups.txt"),
                     delim = "|", escape_double = FALSE, trim_ws = TRUE)

#### Clean interventions ####
drug_intervention <- read_delim(here("0_Data", "AACT", "interventions.txt"),
                                delim = "|", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(drug = ifelse(intervention_type == "Drug",1,0)) 

table(drug_intervention$drug, drug_intervention$intervention_type, useNA = "always") # what is up with the NA?

#### Clean conditions ####
conditions <- read_delim(here("0_Data", "AACT", "conditions.txt"),
                         delim = "|", escape_double = FALSE, trim_ws = TRUE) %>%
  rename(condition = "downcase_name")

#### Load eligibilities and join all study variables ####
elig <- read_delim(here("0_Data", "AACT", "eligibilities.txt"),
                   delim = "|", escape_double = FALSE, trim_ws = TRUE) %>%
  left_join(keywords, by = join_by(nct_id)) %>%
  left_join(randomized, by = join_by(nct_id)) %>%
  left_join(sponsors, by = join_by(nct_id)) %>%
  left_join(drug_intervention, by = join_by(nct_id)) %>%
  left_join(conditions, by = join_by(nct_id)) %>%
  left_join(groups, by = join_by(nct_id)) %>%
  left_join(study, by = join_by(nct_id)) %>%
  left_join(summary, by = join_by(nct_id)) %>%
  filter(drug & randomized) %>%
  mutate(min_age = parse_number(minimum_age),
        max_age = parse_number(maximum_age),
          
          #% check on capitalization
          min_age_unit = case_when(str_detect(minimum_age, "Years") ~ 1,
                                   str_detect(minimum_age, "Months") ~ 1/12,
                                   str_detect(minimum_age, "Weeks") ~ 1/52,
                                   str_detect(minimum_age, "Days") ~ 1/365,
                                   str_detect(minimum_age, "Hours") ~ 1/8760),
          max_age_unit = case_when(str_detect(maximum_age, "Years") ~ 1,
                                   str_detect(maximum_age, "Months") ~ 1/12,
                                   str_detect(maximum_age, "Weeks") ~ 1/52,
                                   str_detect(maximum_age, "Days") ~ 1/365,
                                   str_detect(maximum_age, "Hours") ~ 1/8760),
          min_age = min_age * min_age_unit,
          max_age = max_age * max_age_unit)

  ### ALL TRIALS ###
  elig2.1 = elig %>% 
    left_join(countries, by = join_by(nct_id)) %>%
    group_by(nct_id) %>%
    mutate(US_temp = name%in%c("United States", "United States Minor Outlying Islands", "Virgin Islands (U.S.)", "Puerto Rico",
                              "Guam", "American Samoa"),
           US = sum(US_temp, na.rm = T) >= 1,
           funder_USGOV = sum(agency_class%in%c("FED", "NIH"))>1,
           funder_INDUSTRY = sum(agency_class%in%c("INDUSTRY"))>1,
           funder_NIH = sum(agency_class%in%c("NIH"))>1,
           n = length(unique(name)),
           one_country = name[1],
           all_countries = paste(unique(name), collapse = "+"),
           conditions = paste(unique(condition), collapse = "~"), 
           start_year = year(as.Date(start_date, "%Y-%m-%d")),
           post_year = year(as.Date(study_first_posted_date, "%Y-%m-%d")))
  
  # out for general processing
  elig2.1_out = unique(data.table(elig2.1 %>% 
    dplyr::select(nct_id, official_title, summary,
                  gender, minimum_age, maximum_age, min_age, max_age, criteria, US, n, one_country,
                  all_countries, start_year, post_year, overall_status, start_date, conditions, phase,
                  funder_USGOV, funder_INDUSTRY, funder_NIH, completion_date, enrollment, masking, primary_purpose))) 
  
  # n's for appendix
  n.full = nrow(elig2.1_out)
  n.full.year.limited = nrow(elig2.1_out[elig2.1_out$post_year>=2008 & elig2.1_out$post_year<=2023])
  table(elig2.1_out$gender=="Male")
  table(elig2.1_out$min_age>45)
  table(elig2.1_out$max_age<18)
  n.remain = n.full - table(elig2.1_out$gender=="Male" | elig2.1_out$min_age>45 | elig2.1_out$max_age<18)[2]
  
  # now filter
  elig2.1_out = elig2.1_out %>%
    filter(is.na(gender) | gender!="Male") %>%
    filter(is.na(min_age) | min_age <= 45) %>%
    filter(is.na(max_age) | max_age >= 18) %>%
    filter(US | funder_INDUSTRY | funder_USGOV) 
  
  # add additional variables to determine completion
  elig2.2_out = elig2.1_out %>%
    mutate(start_chk = ifelse(as.Date(start_date, "%Y-%m-%d") + 365 < "2024-04-01", 1, 0),
           start_chk_filter = !start_chk | (start_chk & overall_status!="Not yet recruiting"),
           complete_chk = ifelse(as.Date(completion_date, "%Y-%m-%d") + 365 < "2024-04-01", 1, 0),
           completion_year = year(as.Date(completion_date, "%Y-%m-%d")),
           complete_chk_filter = !complete_chk | (complete_chk & overall_status%in%c("Completed","Terminated", "Suspended")))
  
  write.csv(elig2.2_out, here("0_Data", "OpenAI_Input", "trials_ALL.csv"), row.names = FALSE)
  
####****************** EXPORT FOR CHAT GPT ******************####
  # exporting for chatGPT
  elig2 = elig2.2_out %>% dplyr::select(nct_id, official_title, summary, gender, 
                                    minimum_age, maximum_age, criteria, conditions) 
  
  # combine fields into 1
  elig2_out = unite(elig2, trial_vals, nct_id, official_title, summary, gender, 
                minimum_age, maximum_age, criteria, conditions, sep = " ") 
  
  # Save files
  write.csv(elig2_out, here("0_Data",  "OpenAI_Input", "trials_ALL_combined.csv"), row.names = FALSE)
  
####****************** EXPORT FOR CHAT GPT -- subset ******************####
  
  # set seeds
  set.seed(99999)

  # pull random NCT IDs
  df1 = read.csv(here("0_Data", "Labeled_Data_RA", "preg_enrolled_021324.csv"))
  k1 = sample(df1$nct_id, 600)
  
  df2 = read.csv(here("0_Data", "Labeled_Data_RA", "preg_excluded_021324.csv"))
  k2 = sample(df2$nct_id, 600)
  
  df3 = read.csv(here("0_Data", "Labeled_Data_RA", "preg_notmentioned_021324.csv"))
  k3 = sample(df3$nct_id, 600)
  
  # make subsets
  elig2_sub = elig2 %>% filter(nct_id %in% c(k1, k2, k3))
  
  # combine fields into 1
  elig2_out_sub = unite(elig2_sub, trial_vals, nct_id, official_title, summary, gender, 
                    minimum_age, maximum_age, criteria, conditions, sep = " ") 
  
  # Save files
  write.csv(elig2_out_sub, here("0_Data", "OpenAI_Input", "trials_SUBSET_combined.csv"), row.names = FALSE)
  write.csv(elig2_sub, here("0_Data", "OpenAI_Output", "trials_SUBSET.csv"), row.names = FALSE)
  
####*************** MALE PROCESSING 
  # out for general processing
  elig2.1_MEN = unique(data.table(elig2.1 %>% 
                                    dplyr::select(nct_id, official_title, summary,
                                                  gender, minimum_age, maximum_age, min_age, max_age, criteria, US, n, one_country,
                                                  all_countries, start_year, post_year, overall_status, start_date, conditions, phase,
                                                  funder_USGOV, funder_INDUSTRY, funder_NIH, completion_date, enrollment, masking, primary_purpose))) %>%
    filter(is.na(gender) | gender=="Male") %>%
    filter(US | funder_INDUSTRY | funder_USGOV) 
  
  # add additional variables to determine completion
  elig2.2_MEN = elig2.1_MEN %>%
    mutate(start_chk = ifelse(as.Date(start_date, "%Y-%m-%d") + 365 < "2024-04-01", 1, 0),
           start_chk_filter = !start_chk | (start_chk & overall_status!="Not yet recruiting"),
           complete_chk = ifelse(as.Date(completion_date, "%Y-%m-%d") + 365 < "2024-04-01", 1, 0),
           completion_year = year(as.Date(completion_date, "%Y-%m-%d")),
           complete_chk_filter = !complete_chk | (complete_chk & overall_status%in%c("Completed","Terminated", "Suspended")))
  
  write.csv(elig2.2_MEN, here("0_Data", "OpenAI_Input", "trials_ALL_MEN.csv"), row.names = FALSE)
  
  ####****************** EXPORT FOR CHAT GPT ******************####
  # exporting for chatGPT
  elig2_MEN = elig2.2_MEN %>% dplyr::select(nct_id, official_title, summary, gender, 
                                        minimum_age, maximum_age, criteria, conditions) 
  
  # combine fields into 1
  elig2_MEN = unite(elig2_MEN, trial_vals, nct_id, official_title, summary, gender, 
                    minimum_age, maximum_age, criteria, conditions, sep = " ") 
  
  # Save files
  write.csv(elig2_MEN, here("0_Data",  "OpenAI_Input", "trials_ALL_MEN_combined.csv"), row.names = FALSE)
  