####************************ SETUP ************************####
library(here)
library(tidyverse)
library(tictoc)
library(data.table)
library(future)
library(future.apply)
library(jsonlite)
# remotes::install_github("irudnyts/openai", ref = "r6")
library(openai)

####************************ DATA PROCESSING ************************####

# read in data
df = read.csv(here("3_OpenAI_API_Calls/0_Data", "Input.csv"))

####************************ API SETUP ************************####

# API key -- fill in
api_key <- "YOUR API KEY"

# store api key
Sys.setenv(
  OPENAI_API_KEY = api_key
)

# set up client
client <- OpenAI()

####************************ PROMPT SETUP ************************####

prompt = "You are a helpful assistant designed to output JSON. 
        For each entry, create (i) a variable labeled NCT_ID with the NCT ID, 
        (ii) a variable called AnyPregGPT indicating status of pregnant individuals in the trial.  
        This can take one of 3 values. a) Unspecified: By default, mark a study Unspecified if pregnant 
        individuals were not mentioned in the inclusion or exclusion criteria and/or the trial 
        does not specify inclusion or exclusion based on pregnancy status -- 
        e.g., if pregnancy/lactating/contraceptives/childbearing were not mentioned in
        inclusion or exclusion criteria. 
        b) Included: If and only if pregnant people could explicitly meet inclusion criteria for the clinical trial, 
        mark this field as Included. 
        c) Excluded: If and only if pregnant/lactating people (or in the pregnant stage)
        were explicitly excluded from the clinical trial (including by stating participants must take contraceptives 
        to participate, the study requires a negative pregnancy test, or the trial excludes participants aged 
        18-45 years), mark this field as Excluded. Only mark this field as Excluded based on explicit quotable 
        text related to pregnancy in study description. Studies that only fail to specify inclusion should be 
        marked as Unspecified. 
        (iii) a variable called Summary containing a summary of why you made these classifications. 
        Remember that at trial should not be listed as Excluded on the basis of age unless it enrolls only neonates, 
        infants, children, toddlers, perimenopausal women, or postmenopausal women.  
        It can be listed as Excluded if it only enrolls these groups.  
        If a study enrolls premenopausal women or includes any women aged 18-45, it should not be marked 
        Excluded only on the basis of age. You can mark studies as Excluded if they study IVF, fertility treatments,
        hysterectomy, or contraceptives. Note any concerns about the AnyPregGPT classification. 
        (iv) Summary quote: If a AnyPregGPT is marked Included or Excluded, directly quote the text related to 
        pregnancy that led to this classification from the title, summary, or inclusion criteria. 
        If you cannot, AnyPregGPT should be marked as Unspecified, and this field should say Unspecified.
        (v) a variable called AnyPregGPT_updated that updates AnyPregGPT based on the Summary quote field. 
        If Summary quote is field lists Unspecified, this field should be listed as Unspecified."

####************************ OBTAIN RESULTS ************************####

plan(multisession, workers = 30)  # adjust 'workers' to the number of threads you want

# run call
run_call = function(prompt, item, a){
  # completion
  completion <- client$chat$completions$create(
    model = "gpt-4o",
    temperature = 0,
    messages = list(list("role" = "system", "content" = prompt),
                    list("role" = "user", "content" = item)))
  
  # get output
  out = completion$choices[[1]]$message$content
  
  # return 
  print(a)
  return(out)
}


# use future_lapply to run the loop with multiple threads and store results
out_run = df[1:10,]
tic()
results_list <- future_lapply(1:length(out_run), function(a) {run_call(prompt = prompt, item = out_run[a], a)})
toc()


# parse as json
results_dt = data.table()
for(i in 1:length(results_list)){
  
  # clean it up
  out_cleaned <- gsub("```|json|\n|", "", results_list[[i]])
  out_cleaned <- gsub(",}", "}", out_cleaned)
  
  # parse it
  df_out <- as.data.table(fromJSON(out_cleaned, flatten = TRUE))
  
  # combine
  results_dt <- rbindlist(list(results_dt, df_out), fill = T)
  
  
  print(i)
}

# explore output
dim(results_dt)
View(results_dt)

# write output
write.csv(results_dt, file = here("3_OpenAI_API_Calls/0_Data", "output_R.csv"))
