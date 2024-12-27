#### SETUP ####

# import packages
import os                                 # misc interfaces
from openai import OpenAI                 # call API
import json                               # manipulate json output
import pandas as pd                       # use data frames
from joblib import Parallel, delayed      # parallelize code
import time                               # track runtime
from datetime import datetime             # track time
from threading import Lock                # threading options
import random                             # random number generator

# API key
client = OpenAI(api_key = "YOUR API KEY")

# set up working directory
os.chdir("YOUR WORKING DIRECTORY")

# read in data file
df = pd.read_csv('3_OpenAI_API_Calls/0_Data/input.csv')

# subset data file for now
#df = df.iloc[0:50]
df = df.sample(n=50)
df.shape

# write prompts
prompt = """You are a helpful assistant designed to output JSON. For each entry, create (i) a variable 
               labeled NCT_ID with the NCT ID, 
               (ii) A variable PregGPT equal to Included if the trial Includes pregnant people, 
               Excluded if it explicitly excludes them, and 
               Unspecified if the description does not mention pregnancy.  
               (iii) A variable Summary that provides the reason for this classification.
               (iv) If PregGPT is Included or Excluded, provide a quote that explains this justification.  
               Otherwise, mark this field Unspecified."""

prompt = """You are a helpful assistant designed to output JSON. 
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
        If Summary quote is field lists Unspecified, this field should be listed as Unspecified."""

prompt_check = """You are a helpful assistant designed to output JSON. For each entry, create 
                (i) a variable labeled NCT_ID with the NCT ID, 
                (ii) a variable called 'ChatGPT_Check'.  Read the justification and quote given, and state whether the 
                evidence given indeed suggests pregnant/lactating people are excluded from the trial (Excluded), 
                or whether the evidence given suggests that the trial did not specify whether pregnant/lactating people 
                were included (Unspecified). 
                (iii) Explain why you agree or disagree."""

#### FUNCTION TO CALL API ####
def work(i, item):

    try:
        input_data = str(item)
        
        # make API call
        response = client.chat.completions.create(
              model="gpt-4o",
              response_format={ "type": "json_object" },
              temperature = 0,
              messages=[
               {"role": "system", "content": prompt},
               {"role": "user", "content": input_data}]
        )

        # extract and return output
        out = response.choices[0].message.content

        # print results (can comment out  one or both if you prefer)
        print(i) # print item number
        print(out) # print output

        # return output
        return response.usage.total_tokens, out

    except Exception as ex:
        # prints error
        print("Exception: ",ex)
        return 0,0

#### RATE LIMITING ####
#*** Thanks for this chunk to Luke Massa! ***#
#*** Source: https://github.com/lukemassa/chatgpt-multi-threading/blob/main/main.py ***#

class Tracker:
    def __init__(self, max_rate):
        self.max_rate = max_rate
        self._tokens_per_minute = {}
        self._lock  = Lock()
        self._start = datetime.now()

    def minutes_since_start(self):
        return int((datetime.now() - start).total_seconds() / 60)

    def add(self, tokens):
        minutes = self.minutes_since_start()
        with self._lock:
            self._tokens_per_minute[minutes] = self._tokens_per_minute.get(minutes, 0) + tokens

    def rate(self):
        minutes = self.minutes_since_start()
        with self._lock:
            tokens = self._tokens_per_minute.get(minutes, 0)
            if minutes != 0:
                tokens+=self._tokens_per_minute.get(minutes-1, 0)
            return tokens

    def wait_until_ready(self):
        sleep_time = .001
        while self.rate() > self.max_rate:
            #print("Too fast!")
            time.sleep(random.uniform(sleep_time, sleep_time*2))
            sleep_time*=2

# set global variables
start = datetime.now()
used_tokens = []

#### CALL JOB WITH RATE LIMITS ####
def rate_limited_worker(i, item, tracker):

    # wait
    tracker.wait_until_ready()

    # run job
    tokens, result = work(i, item)

    # output results
    print(f"Got a result, spent {tokens} tokens")
    print(tracker.rate())

    # add tokens
    tracker.add(tokens)

    return result

#### RUN WITHOUT RATE LIMITS ####

#### RUN CODE ####

if __name__ == '__main__':

    # Generate a list of items to process.
    # This has data about each trial.
    items = df['trial_vals']

    # Run in parallel.
    res = Parallel(n_jobs=100, backend = 'threading')(delayed(work)(i, item) for i,item in enumerate(items))
    
#### RUN WITH RATE LIMITS ####

if __name__ == '__main__':

    # Set token limit
    t = Tracker(500000)

    # Generate a list of items to process.
    # This has data about each trial.
    items = df['trial_vals']

    # Run in parallel
    res = Parallel(n_jobs=100, backend = 'threading')(delayed(rate_limited_worker)(i, item, t) for i,item in enumerate(items))

#### STORE OUTPUT ####

# Define the file path for data
path = '3_OpenAI_API_Calls/0_Data/output.csv'

# run for the first value to get headers
json_val = json.loads(res[0])
pandas_val = pd.json_normalize(json_val)
pandas_val.to_csv(path, mode='w', index=False, header=True)

# run over other values
for value in range(len(res)):
    json_val = json.loads(res[value])
    pandas_val = pd.json_normalize(json_val)
    pandas_val.to_csv(path, mode='a', index=False, header=False)

