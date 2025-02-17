### 
# Clean the climate_policies.csv file
#
# To use, just place the climate_policies.csv file in this folder structure:
# data/
#    Climate Policies/
#        climate_policies.csv
###

import pandas as pd

# Load the data
df = pd.read_csv('data/Climate Policies/climate_policies.csv', sep=';')

# if policy_id is not a number, drop the row
df = df[pd.to_numeric(df['policy_id'], errors='coerce').notnull()]
df['policy_id'] = df['policy_id'].astype(int)

# if policy_id is not unique, drop the row
df = df.drop_duplicates(subset='policy_id')

# if country_iso is not a string of length 3, drop the row
df = df[df['country_iso'].str.len() == 3]

# remove columns that are empty
df = df.dropna(axis=1, how='all')

# write to new csv file
df.to_csv('data/Climate Policies/climate_policies_cleaned.csv', index=False)