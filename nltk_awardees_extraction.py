#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 14 12:26:29 2021

1. Load tidy_awards.csv into pandas DV
2. for each [raw] variable, invoke NLTK and extract names
3. prep names (strip punctuation and --> lower for each)
4. output a tiday df with Name, Year, and Section for each entity in each award

@author: nick
"""

import pandas as pd
import spacy

nlp = spacy.load("en_core_web_sm") 

df = pd.read_csv('/home/nick/Dropbox/Docs/Historical_Sociology_Methods_Project_Damon/Socius_Consecration/data/tidy_awards.csv', sep=";")

payload = []

for index, row in df.iterrows():
    buff = nlp(row['Raw'])
    name = ''
    organization = ''
    
    # need to split multiple authors if more than one person and org for each row
    # for tidy purposes, make each person or org its own row (leaving the other blank) and reset the var to blank

    for x in [y for y in buff.ents if y.label_ == 'PERSON']:
        name = x
        payload.append([row['Award_Year'],row['Sect'],row['Award'],row['Type'], name, organization, row['Raw']])
        name = ''
    
    for x in [y for y in buff.ents if y.label_ == 'ORG']:
        organization = x
        payload.append([row['Award_Year'],row['Sect'],row['Award'],row['Type'], name, organization, row['Raw']])
        organization = ''
    
df2 = pd.DataFrame(payload)

df2.to_csv('nlp_output.csv', sep=";")
