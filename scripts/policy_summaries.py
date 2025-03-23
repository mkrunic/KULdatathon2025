

from transformers import pipeline
from tqdm import tqdm
import pandas as pd
import topic_counts

# Load summarization model
summarizer = pipeline("summarization", model="facebook/bart-large-cnn")

summaries = []

df = pd.read_csv('policy_dashboard/data/climate_policies_topics.csv', sep=';')
topics = topic_counts.topic_mapping.keys()
countries = df['country_iso'].unique()
# Summarizing the policy_description values

policies = {country: {topic: [] for topic in topics} for country in countries}

# Step 1: Concatenate policies of the same topic for each country
for index, row in df.iterrows():
    country = row['country_iso']
    for topic in topics:
        if row[topic] >= 1 and pd.notna(row['policy_description']):
            policies[country][topic].append(row['policy_description'])

# Step 2: Summarize the concatenated policies

summaries = []


for country in countries:
    for topic in topics:
        if isinstance(policies[country][topic], list) and len(policies[country][topic]) > 0:
            policies_concat = " ".join(map(str, policies[country][topic]))
            max_len = min(500, len(policies_concat.split(" ")))
            min_len = min(50, len(policies_concat.split(" ")))
            summary = summarizer(policies_concat, max_length=max_len, min_length=min_len, do_sample=False)
            summaries.append({
                'country_iso': country,
                'topic': topic,
                'summary': summary[0]['summary_text']
            })

df_summaries = pd.DataFrame(summaries)
print(df_summaries.head())
df_summaries.to_csv('policy_dashboard/data/policy_summaries.csv', index=False, sep=';')