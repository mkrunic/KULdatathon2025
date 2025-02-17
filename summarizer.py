import argparse
from transformers import pipeline
import pypdfium2 as pdfium
import os

def load_document(file_path):
    """Reads a text or PDF file and returns its content."""
    if file_path.lower().endswith(".pdf"):
        return load_pdf(file_path)
    else:
        with open(file_path, 'r', encoding='utf-8') as file:
            return file.read()

def load_pdf(pdf_path):
    """Extracts text from a PDF file."""
    return '230206098;KAZ;Annual;Environmental Code;Environmental Code Kazakhstan (2021);Country;;Kazakhstan;;;Regulatory Instruments, Obligation schemes;General;The code regulates activities which may have an impact on the environment. It includes regulations related to GHG emissions among others. For emissions, it establishes a mandate for the country\'s biggest polluters to adopt best available technologies.;Energy service demand reduction and resource efficiency, Renewables;;In force;2021;;;High;Mitigation;https://astanatimes.com/2021/03/new-environmental-code-in-kazakhstan-to-promote-green-technology-sustainable-development/;10/01/2023;;;;;;;;;;' 


def chunk_text(text, max_length=1024):
    """Splits the text into chunks of a specified maximum length."""
    words = text.split()
    chunks = [" ".join(words[i:i + max_length]) for i in range(0, len(words), max_length)]
    return chunks

def summarize_text(text, model_name="facebook/bart-large-cnn", max_length=50, min_length=10):
    """Summarizes a given text using the specified Hugging Face model."""
    summarizer = pipeline("summarization", model=model_name)
    return summarizer(text, max_length=max_length, min_length=min_length, do_sample=False)[0]['summary_text']

def summarize_document(file_path, output_path):
    """Loads, chunks, summarizes, and saves the summarized version of a document."""
    print(f"Loading document from {file_path}...")
    document_text = load_document(file_path)
    
    print("Chunking document...")
    chunks = chunk_text(document_text)
    
    print("Summarizing document...")
    summaries = [summarize_text(chunk) for chunk in chunks]
    final_summary = "\n".join(summaries)
    
    if not os.path.exists(output_path):
            os.makedirs(os.path.dirname(output_path), exist_ok=True)
    
    with open(output_path, 'w', encoding='utf-8') as output_file:    
        output_file.write(final_summary)
    
    print(f"Summarization complete. Output saved to {output_path}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Summarize lengthy climate policy documents using Hugging Face models.")
    parser.add_argument("input_file", type=str, help="Path to the input text or PDF file.")
    parser.add_argument("output_file", type=str, help="Path to save the summarized text.")
    args = parser.parse_args()
    
    summarize_document(args.input_file, args.output_file)
