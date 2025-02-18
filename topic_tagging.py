###
# Use zero-shot classification model (source: Hugging Face) to assign topic score to each policy.
# Topics are: Renewable Energy, Carbon Emissions, Carbon Capture, 
#             Waste Management, Sustainable Agriculture,
#             Water Conservation, Public Awareness Initiatives,
#             Carbon Tax, Biodiversity Conservation, Energy Efficiency
###




# Import necessary libraries
from transformers import pipeline
import csv  # Import the CSV module

print("during")

# Load the zero-shot classification model
classifier = pipeline("zero-shot-classification", model="facebook/bart-large-mnli")

# Define the topics
topics = [
    "Renewable Energy", "Carbon Emissions", "Carbon Capture", 
    "Waste Management", "Sustainable Agriculture",
    "Water Conservation", "Public Awareness Initiatives",
    "Carbon Tax", "Biodiversity Conservation", "Energy Efficiency"
]

# Function to assign topic scores to each policy and write to CSV
def load_data(file_path):
    with open(file_path, "r") as file:
        reader = csv.reader(file)
        data = list(reader)

        # only keep the columns policy_id, country_iso, policy_description and start_date (columns 0, 1, 12 and 17)
        data = [[row[0], row[1], row[17], " ".join(row[2:])] for row in data]
    return data


def assign_topic_scores(data, output_csv):
    with open(output_csv, "a", newline="") as file:
        writer = csv.writer(file, delimiter=';')

        for row in data:
            policy_id, country_iso, start_date, policy_description = row
            if policy_id not in output_csv:
                topic_scores = classifier(policy_description, topics)["scores"]
                writer.writerow([policy_id, country_iso, policy_description, start_date] + topic_scores)
            else: continue
    print("here")

    
# Example usage
if __name__ == "__main__":
    file_path = "data/Climate Policies/climate_policies_cleaned.csv"
    output_csv = "data/Climate Policies/topic_scores_output.csv"  # Specify the output CSV file name
    data = load_data(file_path)
    assign_topic_scores(data, output_csv)
    print(f"Topic scores written to {output_csv}")

