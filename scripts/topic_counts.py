###
# Use zero-shot classification model to assign topic score to each policy.
###

# Import necessary libraries
import pandas as pd
import csv  # Import the CSV module

### Topic classification ###
topic_mapping = {
    "Agriculture, Forestry & Land Use": 
        ["Agriculture",
         "Agriculture and forestry", 
         "Forestry"],
    "Buildings & Appliances": 
        ["Buildings",
         "Appliances",
         "Construction", 
         "Heating and cooling", 
         "Hot water and cooking"],
    "Energy Production & Supply": 
        ["Electricity and heat", 
         "Gas", 
         "Oil", 
         "Coal", 
         "Nuclear",
         "Fossil fuel exploration and production",
         "Industrial energy related"],
    "Technologies and Solutions": 
        ["CSS", 
         "Negative emissions", 
         "Renewables", 
         "Energy efficiency"],
    "Transport": 
        ["Light-duty vehicles", 
         "Heavy-duty vehicles", 
         "Rail", 
         "Shipping", 
         "Low-emissions mobility"],
    "Atmospheric Gases": 
        ["Air", 
         "Waste CH4", 
         "Fluorinated gases", 
         "Agricultural CO2", 
         "Agricultural CH4", 
         "Agricultural N2O", 
         "Industrial process CO2", 
         "Industrial N2O"]
}


# Load the data
df = pd.read_csv('policy_dashboard/data/climate_policies_cleaned.csv', sep=';')

# add one column per main topic to the df, fill with zeros
for topic in topic_mapping.keys():
    df[topic] = 0

# for each policy, check if the sector column contains a keyword for a topic
# if so, assign a 1 to the corresponding column
# Now, loop over each row and set the topic columns based on the 'sector' field
for index, row in df.iterrows():
    sectors = [s.strip() for s in str(row['sector']).split(",") if s]  # safely handle missing values
    for topic, keywords in topic_mapping.items():
        for sector in sectors:
            if sector in keywords:
                df.at[index, topic] += 1

df.to_csv('policy_dashboard/data/climate_policies_topics.csv', index=False, sep=';')

# subset the df to keep only the columns with the counts
df_counts = df[["country_iso"] + list(topic_mapping.keys())]

# groups counts by country
df_counts = df_counts.groupby('country_iso').sum().reset_index()

# Calculate total policy count per country
df_counts['total_policies'] = df_counts[list(topic_mapping.keys())].sum(axis=1)

# Normalize topic scores per country
for topic in topic_mapping.keys():
    df_counts[f"{topic}_normalized"] = df_counts[topic] / df_counts['total_policies']

# save csv
df_counts.to_csv('policy_dashboard/data/climate_policies_counts.csv', index=False, sep=';')


 