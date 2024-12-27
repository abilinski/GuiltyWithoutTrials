# source data
source("global_options.R")

#### Load data ####

# load all trials
d = read.csv(here("0_Data", "OpenAI_Input", "trials_ALL.csv"))

# load labeled trials
g = read.csv(here("0_Data", "OpenAI_Output/1_Full_Results", "out_ALL_0_30000.csv")) %>%
  bind_rows(read.csv(here("0_Data", "OpenAI_Output/1_Full_Results", "out_ALL_30001_END.csv")))
length(unique(g$NCT_ID))

  # export for actor-critic
  g_EXCLUDED = g %>% filter(AnyPregGPT_updated=="Excluded") %>%
    dplyr::select(NCT_ID, Summary, Summary_quote) %>% unite(var, NCT_ID, Summary, Summary_quote, sep = " ")
  
  write.csv(g_EXCLUDED, file = here("0_Data", "Post_Checks", "1_Full_Results", "out_v2_FULL_check.csv"))

  # import results for actor-critic
  g_EXCLUDED_REVIEWED = read.csv(here("0_Data", "Post_Checks", "1_Full_Results", "out_v2_FULL_check_RESULTS.csv")) %>%
    unique()
  
  # 336 switched to unspecified
  table(g_EXCLUDED_REVIEWED$ChatGPT_Check)
  
  # merge back
  g1 = g %>% left_join(g_EXCLUDED_REVIEWED) %>%
    mutate(AnyPregGPT_updated = ifelse(ChatGPT_Check=="Unspecified" & !is.na(ChatGPT_Check), "Unspecified", AnyPregGPT_updated))
  
  # Verify that 336 were moved from excluded to unspecified
  table(g1$AnyPregGPT_updated)- table(g$AnyPregGPT_updated)
  
# merge files so that you have full data
l = d %>% left_join(g, c("nct_id" = "NCT_ID"))

#### Filter out trials ####

# filter out unknown status
l1 = l %>% filter(overall_status!="Unknown status")
n.unknown = nrow(l1) - nrow(l)

# filter out withdrawn trials
l2 = l1 %>% filter(overall_status!="Withdrawn")
n.withdrawn = nrow(l2) - nrow(l1)

# make files to check conditions
l2_cat = l2 %>% filter(AnyPregGPT_updated=="Included") %>%
  dplyr::select(nct_id, official_title, summary, gender, minimum_age,
                maximum_age, criteria, conditions) %>%
  mutate(conditions = paste("Conditions: ", conditions)) 
write.csv(l2_cat %>%
            unite(Combined, nct_id, official_title, summary, gender, minimum_age,
                  maximum_age, conditions, criteria), 
          file = here("0_Data", "Category_Classification", "category_input.csv"))

# eval on start/end criteria -- probably not worth it
l3 = l2 %>% filter(start_chk_filter & complete_chk_filter)

#### Check gender and age ####
l2_ga = l2 %>% filter((is.na(min_age) & is.na(max_age)) | is.na(gender))

# write csv
write.csv(l2_ga %>% dplyr::select(nct_id, official_title, summary, gender, minimum_age,
                                   maximum_age, criteria) %>%
            unite(Combined, nct_id, official_title, summary, gender, minimum_age,
                  maximum_age, criteria), 
          file = here("0_Data", "Elig_check", "elig_check_input.csv"))

# merge for review
l2_ga_rev = read.csv(here("0_Data", "Elig_check", "elig_check_output2.csv")) %>%
  filter(Either==TRUE) %>%
  left_join(l2_ga %>% dplyr::select(nct_id, official_title, summary, gender, minimum_age,
                                    maximum_age, criteria), c("NCT_ID" = "nct_id"))

write.csv(l2_ga_rev, file = here("0_Data", "Elig_check", "elig_check_output_REV.csv"))

# read back in
l_ga_rev2 = read.csv(here("0_Data", "Elig_check", "elig_check_output_REV_edited.csv")) %>%
  filter(Check==1)

# now filter l2
dim(l2)
l2 = l2 %>% filter(!nct_id%in%l_ga_rev2$NCT_ID)
dim(l2)

#### Compare to RA classification ####
# read in RA data
ra1 = read.csv(here("0_Data", "Labeled_Data_RA", "preg_enrolled_021324.csv"))
ra2 = read.csv(here("0_Data", "Labeled_Data_RA", "preg_excluded_021324.csv"))
ra3 = read.csv(here("0_Data", "Labeled_Data_RA", "preg_notmentioned_021324.csv"))

# link data
l_ra = l2 %>% mutate(AnyPreg_RA = case_when(nct_id%in%ra1$nct_id~"Included",
                                        nct_id%in%ra2$nct_id~"Excluded",
                                        nct_id%in%ra3$nct_id~"Unspecified",
                                        TRUE~"Missing"))

# explore
#View(l_ra %>% filter(AnyPreg_RA=="Missing"))
table(l_ra$AnyPreg_RA, l_ra$AnyPregGPT)
sum(l_ra$AnyPreg_RA!=l_ra$AnyPregGPT)
sum(l_ra$AnyPreg_RA=="Missing")

# export for NE spot checks
l_comp = l_ra %>% filter(AnyPreg_RA!="Missing")

# write file
write.csv(l_comp %>% filter(AnyPreg_RA!=AnyPregGPT_updated) %>% 
            filter(!Nonviable_GPT) %>%
  dplyr::select("nct_id", "official_title", "summary", "gender", "minimum_age",
                                   "maximum_age", "criteria", "AnyPregGPT_updated", "Summary", "Summary_quote",
                                   "AnyPreg_RA"), file = here("0_Data", "Full_Comparisons", "GPT_VS_RA_FULL.csv"))

# read in classified RA data
l_reviewed = read.csv(here("0_Data", "Full_Comparisons", "GPT_VS_RA_FULL_REVIEWED.csv"))
table(l_reviewed$Who_is_correct)

# subset to ones on which RA is correct
ra_correct = l_reviewed %>% filter(Who_is_correct=="RA") 
table(ra_correct$AnyPreg_RA)

# update l2
  # initial count
  table(l2$AnyPregGPT_updated)

  # make updates
  l2 = l2 %>% left_join(ra_correct %>% dplyr::select(nct_id, AnyPreg_RA), "nct_id") %>%
    mutate(AnyPregGPT_updated = ifelse(!is.na(AnyPreg_RA), AnyPreg_RA, AnyPregGPT_updated))
  
  # checks
  sum(!is.na(l2$AnyPreg_RA))
  table(l2$AnyPregGPT_updated)

#### REVIEW ABORTION CLASSIFICATIONS ####

# save abortion classifications
a = l2 %>% filter(Nonviable_GPT==1) %>% 
    filter(post_year>=2008 & post_year<=2023) %>%
    dplyr::select(nct_id, post_year, official_title, summary, criteria, conditions, Nonviable_GPT, Nonviable_quote)
write.csv(a, here("0_Data", "Full_Comparisons", "abortion_classifications.csv"))

# move over alternative classifications
a2 = read.csv(here("0_Data", "Full_Comparisons", "abortion_classifications_REVIEWED.csv")) %>% filter(Review==0) %>%
  left_join(a %>% dplyr::select(nct_id, post_year)) %>% filter(post_year>=2008 & post_year<=2023)

# update l2
  # check that it's at 86
  table(l2$Nonviable_GPT)
  
  # update
  l2 = l2 %>% mutate(Nonviable_GPT=ifelse(nct_id%in%a2$nct_id, 0, Nonviable_GPT))
  
  # now at 84
  table(l2$Nonviable_GPT)
  
#### REVIEW POSTPARTUM ####
# first check enrolling postpartum
p_comp = l2 %>% left_join(read.csv(here("0_Data", "OpenAI_Output/1_Full_Results", "pp_redo_ALL.csv")), c("nct_id" = "NCT_ID"))
  
# then check postpartum interventions
p_comp2 = p_comp %>% filter(AnyPregGPT_updated=="Included" & C_section_pain == 0) %>%
  filter(Postpartum_GPT!="Neither" | Enrolls_postpartum==1 |  Postpartum_intervention==1) %>%
  filter(Nonviable_GPT==0)
dim(p_comp2 %>% filter(post_year>=2008 & post_year<=2023))
write.csv(p_comp2 %>% dplyr::select(nct_id, official_title, summary, criteria, Postpartum_GPT,
                                    Postpartum_quote.x, Enrolls_postpartum, Postpartum_intervention, Postpartum_quote.y),
          file = here("0_Data", "Full_Comparisons", "pp_comp2.csv"))

# read in reviewed
p_comp2_rev = read.csv(here("0_Data", "Full_Comparisons", "pp_comp2_REVIEWED.csv")) %>% filter(Review==1) %>%
  left_join(p_comp2 %>% dplyr::select(nct_id, post_year)) %>% filter(post_year>=2008 & post_year<=2023)

# update l2
  table(l2$AnyPregGPT_updated)
  
  # update
  l2 = l2 %>% mutate(AnyPregGPT_updated=ifelse(nct_id%in%p_comp2_rev$nct_id, "Excluded", AnyPregGPT_updated))
  
  # now at 84
  table(l2$AnyPregGPT_updated)


#### REVIEW C-SECTION ####
# then check C-section interventions
p_comp3 = p_comp %>% filter(AnyPregGPT_updated=="Included" & C_section_pain == 1 & Nonviable_GPT==0) %>%
    filter(post_year>=2008 & post_year<=2023)

write.csv(p_comp3 %>% dplyr::select(nct_id, official_title, summary, criteria, conditions, C_section_pain, Postpartum_GPT,
                                    Postpartum_quote.x, Enrolls_postpartum, Postpartum_intervention, Postpartum_quote.y),
          file = here("0_Data", "Full_Comparisons", "pp_comp4.csv"))

#### REVIEW FERTILITY AND ED ####

# create ED to review -- all were kept
ed = read.csv(here("0_Data", "OpenAI_Input", "trials_ALL_MEN.csv")) %>% 
  left_join(read.csv(here("0_Data", "OpenAI_Output/1_Full_Results/ed.csv")), c("nct_id" = "NCT_ID")) %>%
  unique() %>% filter(Erectile==1) %>%
  filter(!overall_status%in%c("Unknown status", "Withdrawn")) %>%
  dplyr::select(nct_id, official_title, post_year, summary, criteria, conditions, Erectile, Erectile_summary, Erectile_quote) %>%
  filter(post_year>=2008 & post_year<=2023)

write.csv(ed, file = here("0_Data", "Full_Comparisons", "ed.csv"))

# review fertility
f = l2 %>% 
  left_join(read.csv(here("0_Data", "OpenAI_Output/1_Full_Results/fertility.csv")), c("nct_id" = "NCT_ID")) %>%
  unique() %>% filter(Fertility==1) %>%
  filter(!overall_status%in%c("Unknown status", "Withdrawn")) %>% filter(post_year>=2008 & post_year<=2023) %>%
  dplyr::select(nct_id, post_year, official_title, summary, criteria, conditions, Fertility, Fertility_summary, Fertility_quote)

write.csv(f, file = here("0_Data", "Full_Comparisons", "fertility.csv"))

# review fertility for men
f_men = read.csv(here("0_Data", "OpenAI_Input", "trials_ALL_MEN.csv")) %>% 
  left_join(read.csv(here("0_Data", "OpenAI_Output/1_Full_Results/fertility_MEN.csv")), c("nct_id" = "NCT_ID")) %>%
  unique() %>% filter(Fertility==1) %>%
  filter(!overall_status%in%c("Unknown status", "Withdrawn")) %>% filter(post_year>=2008 & post_year<=2023) %>%
  dplyr::select(nct_id, post_year, official_title, summary, criteria, conditions, Fertility, Fertility_summary, Fertility_quote)

write.csv(f_men, file = here("0_Data", "Full_Comparisons", "fertility_MEN.csv"))

# read in reviewed versions
f_rev = read.csv(here("0_Data", "Full_Comparisons/fertility_REVIEWED.csv")) %>% filter(Review==1) %>%
  left_join(f%>%dplyr::select(nct_id, post_year)) %>%
  bind_rows(read.csv(here("0_Data", "Full_Comparisons/fertility_MEN_REVIEWED.csv")) %>% filter(Review==1) %>%
              left_join(f_men%>%dplyr::select(nct_id, post_year))) %>%
  filter(post_year>=2008 & post_year<=2023)


#### MERGING IN INITIAL CHECKS ####

  
#### NOW CHECKING INCLUDED TRIALS + CATEGORY ####
  
#### ANALYSIS ####
l_out = l2 %>% 
  
  # join in categories
  left_join(read.csv(here("0_Data", "Category_Classification", "category_output.csv")), c("nct_id" = "NCT_ID")) %>%
  
  # update classifications
  mutate(
    # exclude non viable, postpartum, abortion
    AnyPregGPT_updated2 = ifelse(AnyPregGPT_updated=="Included" &  (Nonviable_GPT==1 | nct_id%in%p_comp2_rev$nct_id | nct_id%in%p_comp3$nct_id), 
                                      "Excluded", AnyPregGPT_updated),
    
    # add separate category
    AnyPregGPT_updated3 = ifelse(Nonviable_GPT==1 | nct_id%in%p_comp2$nct_id | nct_id%in%p_comp3$nct_id, "Nonviable/Postpartum", as.character(AnyPregGPT_updated2)))  %>%
  
  mutate(
    # add in missingness for categories
    primary_purpose = ifelse(is.na(primary_purpose), "Missing", primary_purpose),
    masking = ifelse(is.na(masking), "Missing", masking)) %>%
  
  #### filter on year ####
  filter(post_year>=2008 & post_year<=2023)

#### CHECK CODING ####

  #### Load temp data
  lab = read.csv(here("0_Data", "Labeled_Data_FINAL", "trials_manual_FINAL.csv"))

  #### INCLUDED ####
  l_analyze = l_out %>% filter(AnyPregGPT_updated2=="Included") 
  
  #View(l_analyze %>% filter(primary_purpose=="Prevention"))
  inc_orig = nrow(l_analyze)
  
  # write to check
  write.csv(l_analyze %>% 
              dplyr::select("nct_id", "official_title", "summary",  "gender" , "minimum_age" ,"maximum_age" , 
                            "criteria" ,   "AnyPregGPT_updated2", "Summary.x" , "Summary_quote",  "Preg_Only_GPT.x",
                            "Nonviable_GPT",  "Nonviable_quote" , "Category", "Secondary.category", "Summary.y" ), file = here("0_Data", "Full_Comparisons", "included_check_file.csv"))
  
  
  # read in checks
  l_run = read.csv(here("0_Data", "Full_Comparisons", "included_check_file_AB.csv")) %>% rbind(
    read.csv(here("0_Data", "Full_Comparisons", "included_check_file_NE.csv")))
  
  # reduce final sample to account for year restriction
  l_fin = l_run %>%
    filter(nct_id %in% l_analyze$nct_id)
  table(l_fin$AnyPreg_Rev)
  inc_orig = nrow(l_fin)
  
  # update corrections
  table(l_out$AnyPregGPT_updated2)
  l_out = l_out %>% mutate(AnyPregGPT_updated2 = ifelse(nct_id%in%l_fin$nct_id[l_fin$AnyPreg_Rev=="Excluded"], "Excluded", as.character(AnyPregGPT_updated2)),
                           AnyPregGPT_updated2 = ifelse(nct_id%in%l_fin$nct_id[l_fin$AnyPreg_Rev=="Unspecified"], "Unspecified", as.character(AnyPregGPT_updated2)))
  table(l_out$AnyPregGPT_updated2)
  
  # exclude these from final
  l_fin = l_fin %>% filter(AnyPreg_Rev==1)
  
  #### EXCLUDED ####
  set.seed(02903)
  l_exc = l_out %>% filter(AnyPregGPT_updated=="Excluded") 
  exc_orig = nrow(l_exc)
  
  samps = sample(1:nrow(l_exc), 100)
  l_exc_out = l_exc[samps,] %>% dplyr::select("nct_id", "official_title", "summary",  "gender" , "minimum_age" ,"maximum_age" , 
                                          "criteria" ,   "AnyPregGPT_updated2", "Summary.x" , "Summary_quote") %>%
    mutate(ra_comp = nct_id %in% ra2$nct_id) %>% filter(!nct_id%in%lab$nct_id)
  write.csv(l_exc_out, file = here("0_Data", "Full_Comparisons", "excluded_check_file.csv"))
  
  # read in reviewed data
  l_exc_rev = read.csv(here("0_Data", "Full_Comparisons", "excluded_check_file_REVIEWED.csv"))
  l_exc_rev_fix = l_exc_rev %>% filter(Review!="Excluded")
  table(l_exc_rev_fix$Review)
  
  # add to unspecified
  table(l_out$AnyPregGPT_updated2)
  l_out = l_out %>% mutate(AnyPregGPT_updated2 = ifelse(nct_id %in% l_exc_rev_fix$nct_id, "Unspecified", AnyPregGPT_updated2))
  table(l_out$AnyPregGPT_updated2)
  
  #### UNSPECIFIED ####
  l_unsp = l_out %>% filter(AnyPregGPT_updated=="Unspecified") 
  unsp_orig = nrow(l_unsp)
  samps = c(sample(1:nrow(l_unsp), 100), sample(1:nrow(l_unsp), 2))
  l_unsp_out = l_unsp[unique(samps),] %>% dplyr::select("nct_id", "official_title", "summary",  "gender" , "minimum_age" ,"maximum_age" , 
                                              "criteria" ,   "AnyPregGPT_updated2", "Summary.x" , "Summary_quote") %>%
    mutate(ra_comp = nct_id %in% ra3$nct_id) %>% filter(!nct_id%in%lab$nct_id)
  
  write.csv(l_unsp_out, file = here("0_Data", "Full_Comparisons", "unspecified_check_file.csv"))

  # read in reviewed data
  l_unsp_rev = read.csv(here("0_Data", "Full_Comparisons", "unspecified_check_file_REVIEWED.csv"))
  l_unsp_rev_fix = l_unsp_rev %>% filter(Check!="Unspecified")
  table(l_unsp_rev_fix$Check)
  
  # add to Excluded
  table(l_out$AnyPregGPT_updated2)
  l_out = l_out %>% mutate(AnyPregGPT_updated2 = ifelse(nct_id %in% l_unsp_rev_fix$nct_id, "Excluded", AnyPregGPT_updated2),
                           
                           # make factors
                           AnyPregGPT_updated2 = factor(AnyPregGPT_updated2, levels = c("Included", "Excluded", "Unspecified")))
  table(l_out$AnyPregGPT_updated2)
  
  # validation accuracy estimates
  # top excludes overlap in training for included (where all were reviewed)
  # but estimates are the same
  round((3/(inc_orig-170)*inc_orig + nrow(l_exc_rev_fix)/100*exc_orig + nrow(l_unsp_rev_fix)/100*unsp_orig)/(inc_orig+exc_orig+unsp_orig)*100,1)
  round((3 + nrow(l_exc_rev_fix)/100*exc_orig + nrow(l_unsp_rev_fix)/100*unsp_orig)/(inc_orig+exc_orig+unsp_orig)*100,1)
  
#### TABLE 1 ####
run_prop_test = function(l_out, var1 = "AnyPregGPT_updated2", val1 = "Included", var2, val2){
  
  # calculate n
  n = unlist(l_out %>% group_by(!!sym(var1)==val1) %>% summarize(n = n()) %>% dplyr::select(n))
  
  # calculate x
  x = unlist(l_out %>% group_by(!!sym(var1)==val1) %>% filter(!!sym(var2)%in%val2) %>% summarize(n = n()) %>% dplyr::select(n))
  
  # return x
  return(prop.test(x, n)$p.value)
}

# run all prop tests
run_all_prop_tests = function(l_out){
  
  p = c(1, 
        run_prop_test(l_out, var2 = "primary_purpose", val2 = "Prevention"),
        run_prop_test(l_out, var2 = "primary_purpose", val2 = "Treatment"),
        run_prop_test(l_out, var2 = "primary_purpose", val2 = "Basic Science"),
        run_prop_test(l_out, var2 = "funder_INDUSTRY", val2 = TRUE),
        run_prop_test(l_out, var2 = "funder_NIH", val2 = TRUE),
        run_prop_test(l_out, var2 = "US", val2 = TRUE),
        run_prop_test(l_out, var2 = "masking", val2 = "None (Open Label)"),
        run_prop_test(l_out, var2 = "masking", val2 = "Single"),      
        run_prop_test(l_out, var2 = "masking", val2 = c("Double", "Triple", "Quadruple")))   
  
  stars = case_when(p < .001~"**", p < 0.01~"*", TRUE~"")
  
  return(stars)
}

# calculate p-values

# overall
stars = run_all_prop_tests(l_out)
stars2 = run_all_prop_tests(l_out  %>% filter(AnyPregGPT_updated2!="Unspecified"))
stars3 = run_all_prop_tests(l_out  %>% filter(AnyPregGPT_updated2!="Excluded"))

# make by category
t1 = l_out %>% mutate(n = n()) %>% group_by(AnyPregGPT_updated2) %>% 
    summarize(n = paste(comma(n()), " (",  round(n()/n[1]*100), ")", sep = ""),
              Prevention = paste(comma(sum(primary_purpose=="Prevention", na.rm = T)), " (", round(mean(primary_purpose=="Prevention", na.rm = T)*100), ")", sep = ""),
              Treatment = paste(comma(sum(primary_purpose=="Treatment", na.rm = T)), " (", round(mean(primary_purpose=="Treatment", na.rm = T)*100), ")", sep = ""),
              Basic.Science = paste(comma(sum(primary_purpose=="Basic Science", na.rm = T)), " (", round(mean(primary_purpose=="Basic Science", na.rm = T)*100), ")", sep = ""),
              Industry = paste(comma(sum(funder_INDUSTRY)), " (", round(mean(funder_INDUSTRY)*100), ")", sep = ""),
              NIH = paste(comma(sum(funder_NIH)), " (", round(mean(funder_NIH)*100), ")", sep = ""),
              US = paste(comma(sum(US)), " (", round(mean(US)*100), ")", sep = ""),
              Open.Label = paste(comma(sum(masking == "None (Open Label)", na.rm = T)), " (", round(mean(masking == "None (Open Label)", na.rm = T)*100), ")", sep = ""),
              Single = paste(comma(sum(masking == "Single", na.rm = T)), " (", round(mean(masking == "Single", na.rm = T)*100), ")", sep = ""),
              Double.Plus = paste(comma(sum(masking %in%c("Double", "Triple", "Quadruple"), na.rm = T)), " (", round(mean(masking %in%c("Double", "Triple", "Quadruple"), na.rm = T)*100), ")", sep = ""))

# add unspecified + excluded total
t1.1 = l_out %>% mutate(n = n()) %>% filter(AnyPregGPT_updated2 %in%c("Unspecified", "Excluded")) %>% 
  summarize(n = paste(comma(n()), " (",  round(n()/n[1]*100), ")", sep = ""),
            Prevention = paste(comma(sum(primary_purpose=="Prevention", na.rm = T)), " (", round(mean(primary_purpose=="Prevention", na.rm = T)*100), ")", sep = ""),
            Treatment = paste(comma(sum(primary_purpose=="Treatment", na.rm = T)), " (", round(mean(primary_purpose=="Treatment", na.rm = T)*100), ")", sep = ""),
            Basic.Science = paste(comma(sum(primary_purpose=="Basic Science", na.rm = T)), " (", round(mean(primary_purpose=="Basic Science", na.rm = T)*100), ")", sep = ""),
            Industry = paste(comma(sum(funder_INDUSTRY)), " (", round(mean(funder_INDUSTRY)*100), ")", sep = ""),
            NIH = paste(comma(sum(funder_NIH)), " (", round(mean(funder_NIH)*100), ")", sep = ""),
            US = paste(comma(sum(US)), " (", round(mean(US)*100), ")", sep = ""),
            Open.Label = paste(comma(sum(masking == "None (Open Label)", na.rm = T)), " (", round(mean(masking == "None (Open Label)", na.rm = T)*100), ")", sep = ""),
            Single = paste(comma(sum(masking == "Single", na.rm = T)), " (", round(mean(masking == "Single", na.rm = T)*100), ")", sep = ""),
            Double.Plus = paste(comma(sum(masking %in%c("Double", "Triple", "Quadruple"), na.rm = T)), " (", round(mean(masking %in%c("Double", "Triple", "Quadruple"), na.rm = T)*100), ")", sep = ""))

# bind rows
tab = t(bind_rows(t1[1,], t1.1, t1[2:3,]))

# add stars
tab[,2] = paste(tab[,2], c("", stars), sep = "")
tab[,3] = paste(tab[,3], c("", stars2), sep = "")
tab[,4] = paste(tab[,4], c("", stars3), sep = "")

# adjust labels
tab = bind_cols(
  c("", "Total", "Primary purpose", "", "", "Funding", "",
    "Location", "Masking", "", ""),
  c("", "n", "Prevention", "Treatment", "Basic Science", "Industry", "NIH",
    "US", "Open Label", "Single", "Double plus"), tab)
# redo names
names(tab) = c("Variable", tab[1,-1])
tab = tab[-1,]

# overwrite linesep
line_sep_var = c("\\addlinespace", "", "", "\\addlinespace",
                 "", "\\addlinespace", "\\addlinespace", "", "", "")
tab_out = kbl(tab, "simple", booktabs = T, col.names = NULL, linesep = line_sep_var,
              caption = "Trial characteristics by pregnancy inclusion.  **$p < 0.001$, *$p < 0.05$, for z-test of proportions comparing included vs. excluded/unspecified.")  %>%
  add_header_above(c("Variable" = 2, c("Included", "Excluded/Unspecified", "Excluded", "Unspecified")), bold = TRUE) %>%
  kable_classic(full_width = F) %>% 
  kable_styling(font_size = 10) %>%
  column_spec(2, italic = T)

tab_out

# make plots

# join with additional category crosswalk
l_fin2 = l_fin %>% 
  mutate(Category_Rev2 = ifelse(Category_Rev=="", Category, Category_Rev)) %>%
  left_join(read.csv(here("0_Data", "Category_Classification", "condensed_categories.csv")), c("Category_Rev2" = "Original")) 
unique(l_fin2$Category_Rev2[is.na(l_fin2$Preg.code2)])

#View(l_fin %>% filter(Category_Rev==""))

# process data  
k = l_fin2 %>% group_by(Preg.code2) %>% summarize(n = n()) %>% 
  bind_rows(data.frame(Preg.code2 = c("Erectile dysfunction", "Infertility",
                                "Abortion/Fetal demise"), 
                       n = c(nrow(ed), nrow(f_rev), sum(l_out$Nonviable_GPT==1)))) %>%
  group_by(Preg.code2) %>% mutate(n_tot = sum(n)) %>% arrange(-n) %>% 
  mutate(Preg.code2 = str_wrap(Preg.code2, width = 22),
         col = Preg.code2 %in%c("Erectile dysfunction", "Infertility", "Abortion/Fetal demise", "Postpartum"))

# make plot
plot2 = ggplot(k, aes(x = reorder(Preg.code2,n_tot), y = n, fill = col)) + 
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(x = Preg.code2, y = n_tot + 8, label = n_tot)) + 
  scale_y_continuous(limits = c(0, 275), expand = c(0, 0)) + 
  scale_fill_manual("", labels = c("Pregnant participants", "Non-pregnant participants"), values = c("#0072B2",  "grey75")) + labs(x = "", y = "", title = "Number of RCTs by condition") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
        title = element_text(size = 9)) + coord_flip() 


theme = theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# plot with preg types
p1 = l_out %>% group_by(post_year) %>% 
  filter(post_year >= 2007 & post_year < 2024) %>%
  group_by(post_year) %>% mutate(n_tot = n()) %>%
  group_by(post_year, AnyPregGPT_updated2) %>% 
  summarize(perc = n()/n_tot[1]*100) %>% 
  group_by(post_year) %>% mutate(perc_tot = sum(perc)) 

plot1 = ggplot(p1 %>% ungroup(), 
       aes(x = as.numeric(post_year), y = perc, group = paste(AnyPregGPT_updated2), 
           col = AnyPregGPT_updated2, pch = AnyPregGPT_updated2)) + 
  geom_line() + geom_point(size = 2) + 
  scale_color_manual("", values = c("#0072B2",  "grey60", "grey75")) + 
  scale_shape("") + 
  scale_y_continuous(lim = c(0, 85), expand = c(0,0)) +
  labs(x = "", y = "", title = "Percentage of RCTs by pregnancy inclusion status") + theme_bw() + theme +
  theme(legend.position="bottom", legend.text = element_text(size = 10),
        title = element_text(size = 9), legend.margin = margin(0, 0, 0, 0))


#### TRIAL WATERFALL PLOT ####
trials <- tribble(~restriction, ~color, ~n_trials, ~text_color,
                  "All drug RCTs", "All drug RCTs", 90860, "white",
                  "Did not meet\nsample criteria", "Did not meet sample criteria", 46700, "black",
                  "Excluded\npregnant\nparticipants", "Met sample criteria", 33249, "black",
                  "Unspecified", "Met sample criteria", 10549, "black",
                  "Included\npregnant\nparticipants", "Enrolled pregnant participants", 362, "blue") %>%
  mutate(color = factor(color, levels = c("All drug RCTs", "Did not meet sample criteria",
                                          "Met sample criteria", "Enrolled pregnant participants")))

trials <- trials %>%
  mutate(time = row_number(),
         subtrials = ifelse(restriction == "All drug RCTs", 0, n_trials),
         end = ifelse(restriction == "All drug RCTs", n_trials, 90860-cumsum(subtrials)),
         start = ifelse(time == 1, 0, lag(end)),
         text_loc = (end+start)/2)

plot3 = trials %>%
  ggplot(aes(x = time, y = end)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_rect(aes(xmin = time - 0.45, xmax = time + 0.45, ymin = start, ymax = end, fill = color)) +
  #geom_text(aes(x = time, y = pmin(end, start)- 1200, label =  comma(n_trials))) +
  geom_text(aes(x = time, y = text_loc + 2000, color = text_color, label =  comma(n_trials))) +
  labs(x = "", y = "# of trials", title = "ClinicalTrials.gov drug RCT breakdown (2008-2023)") +
  scale_x_continuous("", breaks = trials$time, labels = trials$restriction) +
  scale_fill_manual(values = c("black","grey40", "grey80", "#0072B2")) +
  scale_color_manual(values = c("black", "#0072B2", "white")) +
  guides(fill = "none", color = "none") +
  theme_bw() + theme + theme(title = element_text(size = 9)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 95000), labels = comma) +
  stat_brace(data = data.frame(x = 2.5:5.5, y = c(44000, 35000, 40000, 40000)), aes(x=x, y=y)) +
  geom_text(data = data.frame(x = 4, y = 52000, text = "Met sample criteria (women 18-45)"), aes(x = x, y = y, label = text))


# compile figure
f1 = ggarrange(plot3, plot1, ncol = 2, widths = c(4,3))
f2 = ggarrange(f1, plot2, ncol = 1)
ggsave(here("2_Output", "trials_figure1.png"), f2, width = 9, height = 8)

# additional statistics

# general stats
l_out %>% mutate(n = n()) %>% 
  group_by(AnyPregGPT_updated2) %>% summarize(n(), n()/n[1])

# 2023 stats
l_out %>% filter(post_year==2023) %>% mutate(n = n()) %>% 
  group_by(AnyPregGPT_updated2) %>% summarize(n(), n()/n[1])

# pregnancy only
l_fin2 %>% summarize(sum(Preg_Only_Rev), mean(Preg_Only_Rev))

# category 
l_fin2 %>% mutate(n = n()) %>% group_by(Preg.code2) %>%
  summarize(n_cat = n(), perc = round(n()/n[1]*100)) %>% arrange(-n_cat)

#View(l_fin2 %>% mutate(n = n()) %>% group_by(Category_Rev2) %>%
#  summarize(n_cat = n(), perc = round(n()/n[1]*100)) %>% arrange(-n_cat))
