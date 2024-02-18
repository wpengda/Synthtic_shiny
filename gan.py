import pandas as pd
import argparse
from ctgan import CTGAN

# Setting Command Line Parameters
parser = argparse.ArgumentParser(description="Run CTGAN")
parser.add_argument('file_path', type=str, help='Path to the input CSV file')
parser.add_argument('num_samples', type=int, help='Number of samples to generate')
args = parser.parse_args()

# retrieve data
real_data = pd.read_csv(args.file_path)

# Define CTGAN
ctgan = CTGAN(epochs=1500)
ctgan.fit(real_data)

# Generating synthetic data
synthetic_data = ctgan.sample(args.num_samples)

# Option to save synthetic data to a file or perform other processing
synthetic_data.to_csv("synthetic_data_gan.csv", index=False)


