import pandas as pd
import argparse
from sdv.single_table import CTGANSynthesizer
from sdv.metadata import SingleTableMetadata

# Setting Command Line Parameters
parser = argparse.ArgumentParser(description="Run CTGAN")
parser.add_argument('file_path', type=str, help='Path to the input CSV file')
parser.add_argument('num_samples', type=int, help='Number of samples to generate')
args = parser.parse_args()

# retrieve data
real_data = pd.read_csv(args.file_path)
metadata = SingleTableMetadata()
metadata.detect_from_dataframe(real_data)

# Define CTGAN
synthesizer = CTGANSynthesizer(
    metadata, # required
    enforce_min_max_values=True,
    enforce_rounding=True,
    epochs=1500,
    verbose=True,
    cuda=True
)

synthesizer.fit(real_data)

# Generating synthetic data
synthetic_data = synthesizer.sample(args.num_samples)

# Option to save synthetic data to a file or perform other processing
synthetic_data.to_csv("synthetic_data_gan.csv", index=False)


