

from transformers import BartTokenizer, BartForConditionalGeneration
from tqdm import tqdm
import pandas as pd
import topic_counts
import torch

# Load summarization model
model_name = "facebook/bart-large-cnn"
tokenizer = BartTokenizer.from_pretrained(model_name)
model = BartForConditionalGeneration.from_pretrained(model_name)

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
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model = model.to(device)

summaries = []

# add column names to csv
file = 'policy_dashboard/data/policy_summaries.csv'
with open(file, 'w') as f:
    f.write("country_iso;topic;summary\n")


for country in countries:
    for topic in topics:
        if isinstance(policies[country][topic], list) and len(policies[country][topic]) > 0:
            policies_concat = " ".join(map(str, policies[country][topic]))

            # Tokenize and truncate to max 1024 tokens
            inputs = tokenizer(
                policies_concat,
                return_tensors="pt",
                truncation=True,
                max_length=1024
            )
            # Move tensors to device (CPU or GPU)
            input_ids = inputs['input_ids'].to(device)
            attention_mask = inputs['attention_mask'].to(device)

            # Make sure the model gets a single sequence (not batch of 2+)
            if input_ids.shape[0] != 1:
                input_ids = input_ids[:1]
                attention_mask = attention_mask[:1]

            max_len = min(500, len(inputs["input_ids"]))
            min_len = min(50, len(inputs["input_ids"]))

            summary_ids = model.generate(
                input_ids=input_ids,
                attention_mask=attention_mask,
                max_length=300,      # output summary length
                min_length=50,
                length_penalty=2.0,
                num_beams=4,
                early_stopping=True,
            )

            summary_text = tokenizer.decode(summary_ids[0], skip_special_tokens=True)


            summaries.append({
                'country_iso': country,
                'topic': topic,
                'summary': summary_text
            })

            # write to csv
            with open(file, 'a') as f:
                f.write(f"{country};{topic};{summary_text}\n")


df_summaries = pd.DataFrame(summaries)
print(df_summaries.head())